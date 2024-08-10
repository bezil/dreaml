(lang dune 3.16)

(name DreAML)

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
 (depends ocaml dune dream)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
