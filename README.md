# ocamlbin

Because rewriting simple versions of standard UNIX utilities is a great way
to learn a new language...


    host$ opam switch create . ocaml.5.1.2
    host$ opam switch list
    host$ eval $(opam env)
    host$ opam install \
        ctypes-foreign \
        ocamlfind  \
        utop


    host$ make
