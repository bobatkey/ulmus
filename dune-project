(lang dune 3.7)

(name ulmus)

(generate_opam_files true)

; (source
;  (github username/reponame))

(authors "Robert Atkey")

(maintainers "Robert Atkey")

(license GPL)

; (documentation https://url/to/documentation)

(package
 (name ulmus)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune js_of_ocaml-ppx sexplib ppx_sexp_conv html_sig base64)
 (tags
  (topics "to describe" your project)))

(package
 (name html_sig))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
