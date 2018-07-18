module Version = Package.Version
module Source = Package.Source
module Req = Package.Req

module Record = struct

  module Opam = struct
    type t = {
      name : Package.Opam.OpamName.t;
      version : Package.Opam.OpamVersion.t;
      opam : Package.Opam.OpamFile.t;
      override : Package.OpamOverride.t option;
    } [@@deriving yojson]
  end

  module Source = struct
    type t = Package.Source.t * Package.Source.t list

    let to_yojson = function
      | main, [] -> Package.Source.to_yojson main
      | main, mirrors -> `List (List.map ~f:Package.Source.to_yojson (main::mirrors))

    let of_yojson (json : Json.t) =
      let open Result.Syntax in
      match json with
      | `String _ ->
        let%bind source = Package.Source.of_yojson json in
        return (source, [])
      | `List _ ->
        begin match%bind Json.Parse.list Package.Source.of_yojson json with
        | main::mirrors -> return (main, mirrors)
        | [] -> error "expected a non empty array or a string"
        end
      | _ -> error "expected a non empty array or a string"

  end

  type t = {
    name: string;
    version: Version.t;
    source: Source.t;
    files : Package.File.t list;
    opam : Opam.t option;
  } [@@deriving yojson]

  let compare a b =
    let c = String.compare a.name b.name in
    if c = 0
    then Version.compare a.version b.version
    else c

  let equal a b =
    String.equal a.name b.name && Version.equal a.version b.version

  let pp fmt record =
    Fmt.pf fmt "%s@%a" record.name Version.pp record.version

  module Map = Map.Make(struct type nonrec t = t let compare = compare end)
  module Set = Set.Make(struct type nonrec t = t let compare = compare end)
end

module Id = struct
  type t = string * Package.Version.t [@@deriving (ord, eq)]

  let rec parse v =
    let open Result.Syntax in
    match Astring.String.cut ~sep:"@" v with
    | Some ("", name) ->
      let%bind name, version = parse name in
      return ("@" ^ name, version)
    | Some (name, version) ->
      let%bind version = Version.parse version in
      return (name, version)
    | None -> Error "invalid id"

  let to_yojson (name, version) =
    `String (name ^ "@" ^ Version.toString version)

  let of_yojson = function
    | `String v -> parse v
    | _ -> Error "expected string"

  let ofRecord (record : Record.t) =
    record.name, record.version

  module Set = Set.Make(struct
    type nonrec t = t
    let compare = compare
  end)

  module Map = struct
    include Map.Make(struct
      type nonrec t = t
      let compare = compare
    end)

    let to_yojson v_to_yojson map =
      let items =
        let f (name, version) v items =
          let k = name ^ "@" ^ Version.toString version in
          (k, v_to_yojson v)::items
        in
        fold f map []
      in
      `Assoc items

    let of_yojson v_of_yojson =
      let open Result.Syntax in
      function
      | `Assoc items ->
        let f map (k, v) =
          let%bind k = parse k in
          let%bind v = v_of_yojson v in
          return (add k v map)
        in
        Result.List.foldLeft ~f ~init:empty items
      | _ -> error "expected an object"
  end
end

[@@@ocaml.warning "-32"]
type solution = t

and t = {
  root : Id.t option;
  records : Record.t Id.Map.t;
  dependencies : Id.Set.t Id.Map.t;
} [@@deriving eq]

let root sol =
  match sol.root with
  | Some id -> Id.Map.find_opt id sol.records
  | None -> None

let dependencies (r : Record.t) sol =
  let id = Id.ofRecord r in
  match Id.Map.find_opt id sol.dependencies with
  | None -> Record.Set.empty
  | Some ids ->
    let f id set =
      let record =
        try Id.Map.find id sol.records
        with Not_found ->
          let msg =
            Format.asprintf
              "inconsistent solution, missing record for %a"
              Fmt.(pair ~sep:(unit "@") string Version.pp) id
          in
          failwith msg
      in
      Record.Set.add record set
    in
    Id.Set.fold f ids Record.Set.empty

let records sol =
  let f _k record records = Record.Set.add record records in
  Id.Map.fold f sol.records Record.Set.empty

let empty = {
  root = None;
  records = Id.Map.empty;
  dependencies = Id.Map.empty;
}

let add ~(record : Record.t) ~dependencies sol =
  let id = Id.ofRecord record in
  let dependencies = Id.Set.of_list dependencies in
  {
    sol with
    records = Id.Map.add id record sol.records;
    dependencies = Id.Map.add id dependencies sol.dependencies;
  }

let addRoot ~(record : Record.t) ~dependencies sol =
  let sol = add ~record ~dependencies sol in
  let id = Id.ofRecord record in
  {sol with root = Some id;}

let dependenciesHash (manifest : Manifest.Root.t) =
  let hashDependencies ~prefix ~dependencies digest =
    let f digest req =
     Digest.string (digest ^ "__" ^ prefix ^ "__" ^ Req.toString req)
    in
    List.fold_left
      ~f ~init:digest
      dependencies
  in
  let hashResolutions ~resolutions digest =
    let f digest (key, version) =
     Digest.string (digest ^ "__" ^ key ^ "__" ^ Version.toString version)
    in
    List.fold_left
      ~f ~init:digest
      (Package.Resolutions.entries resolutions)
  in
  let digest =
    Digest.string ""
    |> hashResolutions
      ~resolutions:manifest.Manifest.Root.resolutions
    |> hashDependencies
      ~prefix:"dependencies"
      ~dependencies:manifest.manifest.dependencies
    |> hashDependencies
      ~prefix:"devDependencies"
      ~dependencies:manifest.manifest.devDependencies
  in
  Digest.to_hex digest

module LockfileV1 = struct

  type t = {
    hash : string;
    root : Id.t;
    node : node Id.Map.t
  }

  and node = {
    record : Record.t;
    dependencies : Id.t list;
  } [@@deriving yojson]

  let mapRecord ~f (record : Record.t) =
    let version =
      match record.version with
      | Version.Source (Source.LocalPath p) ->
        Version.Source (Source.LocalPath (f p))
      | Version.Npm _
      | Version.Opam _
      | Version.Source _ -> record.version
    in
    let source =
      let f source =
        match source with
        | Source.LocalPathLink p ->
          Source.LocalPathLink (f p)
        | Source.LocalPath p ->
          Source.LocalPath (f p)
        | Source.Archive _
        | Source.Git _
        | Source.Github _
        | Source.NoSource -> source
      in
      let main, mirrors = record.source in
      let main = f main in
      let mirrors = List.map ~f mirrors in
      main, mirrors
    in
    {record with source; version}

  let relativize ~cfg record =
    let f path =
      if Path.equal path cfg.Config.basePath
      then Path.(v ".")
      else match Path.relativize ~root:cfg.Config.basePath path with
      | Some path -> path
      | None -> path
    in
    mapRecord ~f record

  let derelativize ~cfg record =
    let f path = Path.(cfg.Config.basePath // path |> normalize) in
    mapRecord ~f record

  let solutionOfLockfile ~cfg root node =
    let f id {record; dependencies} sol =
      let record = derelativize ~cfg record in
      if Id.equal root id
      then addRoot ~record ~dependencies sol
      else add ~record ~dependencies sol
    in
    Id.Map.fold f node empty

  let lockfileOfSolution ~cfg (sol : solution) =
    let node =
      let f id record nodes =
        let record = relativize ~cfg record in
        let dependencies = Id.Map.find id sol.dependencies in
        Id.Map.add id {record; dependencies = Id.Set.elements dependencies} nodes
      in
      Id.Map.fold f sol.records Id.Map.empty
    in
    let root =
      match sol.root with
      | Some root -> root
      | None -> failwith "empty solution"
    in
    root, node

  let ofFile ~cfg ~(manifest : Manifest.Root.t) (path : Path.t) =
    let open RunAsync.Syntax in
    if%bind Fs.exists path
    then
      let%lwt lockfile =
        let%bind json = Fs.readJsonFile path in
        RunAsync.ofRun (Json.parseJsonWith of_yojson json)
      in
      match lockfile with
      | Ok lockfile ->
        if lockfile.hash = dependenciesHash manifest
        then
          let solution = solutionOfLockfile ~cfg lockfile.root lockfile.node in
          return (Some solution)
        else return None
      | Error err ->
        let msg =
          let path =
            Option.orDefault
              ~default:path
              (Path.relativize ~root:cfg.Config.basePath path)
          in
          Format.asprintf
            "corrupted %a lockfile@\nyou might want to remove it and install from scratch@\nerror: %a"
            Path.pp path Run.ppError err
        in
        error msg
    else
      return None

  let toFile ~cfg ~(manifest : Manifest.Root.t) ~(solution : solution) (path : Path.t) =
    let root, node = lockfileOfSolution ~cfg solution in
    let hash = dependenciesHash manifest in
    let lockfile = {hash; node; root} in
    let json = to_yojson lockfile in
    Fs.writeJsonFile ~json path
end
