(lang dune 3.16)

(name hello)

(generate_opam_files true)

(source
 (github bezil/dreaml))

(authors "bezil")

(maintainers "Basil")

(license LICENSE)

(documentation https://aantron.github.io/dream/)

(package
 (name hello)
 (synopsis "Dream OCAML Webapp")
 (description "Learn Dream OCAML Web Framework")
 (depends ocaml dune dream ppx_yojson_conv)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
