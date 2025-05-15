(lang dune 3.17)

(name visualize4d)

(generate_opam_files true)

(source
 (github johnridesabike/v4d))

(authors "John Jackson <jbpjackson@icloud.com>")

(license MPL-2.0)

(package
 (name visualize4d)
 (depends ocaml js_of_ocaml-compiler vg gg brr))
