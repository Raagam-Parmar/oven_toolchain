(lang dune 3.20)
(using menhir 3.0)

(name tiny8)

(generate_opam_files true)

(source
 (github Raagam-Parmar/tiny8))

(authors "Raagam Parmar <raagamparmar@gmail.com>")

(maintainers "Raagam Parmar <raagamparmar@gmail.com>")

(license LICENSE)

(package
 (name tiny8)
 (synopsis "Toolchain for my 8-bit ISA, tiny8")
 (depends ocaml)
 (tags
  ("tiny8")))

