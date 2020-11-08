let esyStoreVersion = "3";
open RunAsync.Syntax;

module Path = {
  include Path;
  let join = (x, y) => Filename.concat(x, y) |> Path.v;
};

let storeBuildTree = "b";
let storeInstallTree = "i";
let storeStageTree = "s";
let cwd = Sys.getcwd();
let releasePackagePath = cwd;
let releaseExportPath = Path.(v(releasePackagePath) / "_export");
let releaseBinPath = Path.(v(releasePackagePath) / "bin");
let unpaddedStorePath = Path.(v(releasePackagePath) / esyStoreVersion);

let getStorePathForPrefix = (prefix, ocamlPkgName, ocamlVersion) => {
  let ocamlrunStorePath =
    ocamlPkgName ++ "-" ++ ocamlVersion ++ "-########/bin/ocamlrun";

  let esyStorePaddingLength =
    127
    - String.length("!#")
    - String.length("/" ++ "i" ++ "/" ++ ocamlrunStorePath);
  let prefixLength = String.length(prefix ++ "/" ++ esyStoreVersion);
  let paddingLength = esyStorePaddingLength - prefixLength;
  if (paddingLength < 0) {
    failwith(
      "Esy prefix path is too deep in the filesystem, Esy won't be able to relocate artefacts",
    );
  };
  let p = Path.join(prefix, esyStoreVersion);
  print_endline("[P]: " ++ Path.show(p));
  Path.v(
    Path.show(p)
    ++ String.make(esyStorePaddingLength - String.length(Path.show(p)), '_'),
  );
};

type fileStat = {
  relative: Fpath.t,
  basename: Fpath.t,
  absolute: Fpath.t,
  mtime: float,
  stats: Lwt_unix.stats,
};

let fsWalk = (~dir) => {
  open RunAsync.Syntax;
  let rec inner = (~path, ~relativePath, ~dirsInPath, ~acc) => {
    switch (dirsInPath) {
    | [] => Lwt.return(Ok(acc))
    | [currentDir, ...restDir] =>
      let currentDirPath = Path.(path / currentDir);
      let basename = Fpath.v(Fpath.basename(currentDirPath));
      let currentRelativePath =
        relativePath
        |> Option.map(~f=relativePath =>
             Fpath.append(relativePath, basename)
           )
        |> Option.orDefault(~default=basename);

      let%bind (isDir, stats) =
        Let_syntax.both(Fs.isDir(currentDirPath), Fs.stat(currentDirPath));

      let file = {
        relative: currentRelativePath,
        basename,
        absolute: currentDirPath,
        mtime: Unix.(stats.st_mtime),
        stats,
      };

      if (isDir) {
        let%bind dirsInCurrentDirPath = Fs.listDir(currentDirPath);
        inner(
          ~path=currentDirPath,
          ~relativePath=Some(currentRelativePath),
          ~dirsInPath=dirsInCurrentDirPath,
          ~acc=[file, ...acc],
        );
      } else {
        inner(
          ~path,
          ~relativePath,
          ~dirsInPath=restDir,
          ~acc=[file, ...acc],
        );
      };
    };
  };
  let%bind dirsInPath = Fs.listDir(dir);
  inner(~path=dir, ~relativePath=None, ~dirsInPath, ~acc=[]);
};

let main = (ocamlPkgName, ocamlVersion, rewritePrefix) => {
  print_endline("[ocamlPkgName]: " ++ ocamlPkgName);
  print_endline("[ocamlVersion]: " ++ ocamlVersion);
  print_endline("[rewritePrefix]: " ++ string_of_bool(rewritePrefix));

  let _check = () => {
    let%lwt buildFound = Fs.exists(releaseExportPath);
    switch (buildFound) {
    | Ok(true) =>
      if (!rewritePrefix) {
        Lwt.return(Ok());
      } else {
        let storePath =
          getStorePathForPrefix(
            releasePackagePath,
            ocamlPkgName,
            ocamlVersion,
          );
        let%lwt storeFound = Fs.exists(storePath);
        Lwt.return(
          switch (storeFound) {
          | Ok(true) => Error(`ReleaseAlreadyInstalled)
          | Ok(false) => Ok()
          | Error(err) => Error(`EsyLibError(err))
          },
        );
      }
    | Ok(false) => Lwt.return(Error(`NoBuildFound))
    | Error(err) => Lwt.return(Error(`EsyLibError(err)))
    };
  };

  let _initStore = () => {
    open Lwt_result;
    let storePath =
      if (rewritePrefix) {
        getStorePathForPrefix(releasePackagePath, ocamlPkgName, ocamlVersion);
      } else {
        unpaddedStorePath;
      };

    Fs.createDir(storePath)
    >>= (
      _ =>
        RunAsync.List.waitAll([
          Fs.createDir(Path.(storePath / storeBuildTree)),
          Fs.createDir(Path.(storePath / storeInstallTree)),
          Fs.createDir(Path.(storePath / storeStageTree)),
        ])
    )
    |> Lwt_result.map_err(err => `EsyLibError(err));
  };

  let doImport = releaseExportPath => {
    let importBuilds = () => {
      print_endline(">>>>>>>>>>>>>>>>>>>>");
      let%bind entries = fsWalk(~dir=releaseExportPath);
      entries |> List.iter(~f=f => print_endline(Path.show(f.absolute)));
      print_endline("<<<<<<<<<<<<<<");
      RunAsync.return();
    };
    importBuilds();
  };
  // module Let_syntax = {
  //   let bind = (~f, v) => Lwt_result.bind(v, f);
  // };
  // let%bind _ = check();
  // initStore();
  /* Lwt_result.(check() >>= (_ => initStore())); */
  let%lwt r = doImport(releaseExportPath);
  switch (r) {
  | Ok () => RunAsync.return()
  | Error(err) =>
    print_endline("Error: " ++ Run.formatError(err));
    RunAsync.return();
  };
};

open Cmdliner;

let ocamlPkgName = {
  let doc = "OCaml package name";
  Arg.(
    value
    & opt(string, "ocaml")
    & info(["ocaml-pkg-name"], ~docv="RELEASE CONFIG", ~doc)
  );
};

let ocamlVersion = {
  let doc = "OCaml package version";
  Arg.(
    value
    & opt(string, "n.00.0000")
    & info(["ocaml-version"], ~docv="RELEASE CONFIG", ~doc)
  );
};

let rewritePrefix = {
  let doc = "Whether to rewrite prefixes in the binary or not";
  Arg.(
    value
    & opt(bool, true)
    & info(["rewrite-prefix"], ~docv="RELEASE CONFIG", ~doc)
  );
};

let lwt_main = (ocamlPkgName, ocamlVersion, rewritePrefix) => {
  /* let res = Lwt_main.run(main(ocamlPkgName, ocamlVersion, rewritePrefix)); */
  /* switch (res) { */
  /* | Ok(_) => print_endline("tout ok") */
  /* | Error(`NoBuildFound) => print_endline("No build found!") */
  /* | Error(`ReleaseAlreadyInstalled) => */
  /*   print_endline("Release already installed!") */
  /* | Error(`EsyLibError(err)) => print_endline(EsyLib.Run.formatError(err)) */
  /* }; */
  Lwt_main.run(
    main(ocamlPkgName, ocamlVersion, rewritePrefix),
  );
};

let main_t =
  Term.(const(lwt_main) $ ocamlPkgName $ ocamlVersion $ rewritePrefix);

let info = {
  let doc = "Export native builds and rewrite prefixes in them";
  let man = [`S(Manpage.s_bugs), `P("")];

  Term.info(
    "NatEsyInstallRelease",
    ~version="0.0.0",
    ~doc,
    ~exits=Term.default_exits,
    ~man,
  );
};

let () = Term.exit @@ Term.eval((main_t, info));
