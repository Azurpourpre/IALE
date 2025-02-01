(lang dune 3.0)

(name IALE)

(generate_opam_files true)

(source
 (github azurpourpre/IALE))

(authors "azurpourpre@proton.me")

(package
 (name IALE)
 (synopsis "Industrial Assertion and Logic Extractor")
 (description "A translator for IEC 61131 languages to Why3ML. Primarly Focused on functionnal specification formal verification")
 (depends ocaml xml-light why3 re)
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
