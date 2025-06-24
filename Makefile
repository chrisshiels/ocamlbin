ALL=cat cmp date echo env id ncat printenv pwd tee


all: ${ALL}


clean:
	rm -f ${ALL}


cat: cat.ml
	ocamlopt cat.ml -o cat


cmp: cmp.ml
	ocamlopt cmp.ml -o cmp


date: date.ml
	ocamlopt -I +unix unix.cmxa date.ml -o date


echo: echo.ml
	ocamlopt echo.ml -o echo


env: env.ml
	ocamlopt -I +unix unix.cmxa env.ml -o env


id: id.ml
	ocamlopt -I +unix unix.cmxa id.ml -o id


ncat: ncat.ml
	ocamlopt -I +unix unix.cmxa ncat.ml -o ncat


printenv: printenv.ml
	ocamlopt -I +unix unix.cmxa printenv.ml -o printenv


pwd: pwd.ml
	ocamlopt pwd.ml -o pwd


tee: tee.ml
	ocamlopt tee.ml -o tee
