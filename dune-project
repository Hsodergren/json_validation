(lang dune 2.8)
(name json_editor)
(version 0.1)
(maintainers "asd")

(generate_opam_files true)

(package
 (name json_editor)
 (synopsis "test")
 (description "asd")
 (depends
  (yojson (>= 1.7.0))
  (re (>= 1.9.0))
  (brr (>= 0.0.1))
  (jsch (>= 0.0.1))
  (js_of_ocaml (>= 3.9.0))
  (react (>= 1.2.1))))
