open Ocamlbuild_plugin

let _ =
   dispatch (function
     | Before_options ->
        Options.use_ocamlfind := true
     | _ -> ())
