ALL=\
	cat cmp date dmesg echo env expr free id ncat pagesize printenv pwd \
	tee tty uname uptime


all: ${ALL}


clean:
	rm -f ${ALL}


cat: cat.ml
	ocamlopt cat.ml -o cat


cmp: cmp.ml
	ocamlopt cmp.ml -o cmp


date: date.ml
	ocamlopt -I +unix unix.cmxa date.ml -o date


dmesg: dmesg.ml
	ocamlopt -I +unix unix.cmxa dmesg.ml -o dmesg


echo: echo.ml
	ocamlopt echo.ml -o echo


env: env.ml
	ocamlopt -I +unix unix.cmxa env.ml -o env


expr: expr.ml
	ocamlopt -I +str str.cmxa expr.ml -o expr


free: free.ml
	ocamlopt -I +unix unix.cmxa free.ml -o free


id: id.ml
	ocamlopt -I +unix unix.cmxa id.ml -o id


ncat: ncat.ml
	ocamlopt -I +unix unix.cmxa ncat.ml -o ncat


pagesize: pagesize.ml
	ocamlfind ocamlopt \
		-linkpkg \
		-package ctypes,ctypes-foreign \
		pagesize.ml -o pagesize


printenv: printenv.ml
	ocamlopt -I +unix unix.cmxa printenv.ml -o printenv


pwd: pwd.ml
	ocamlopt pwd.ml -o pwd


tee: tee.ml
	ocamlopt tee.ml -o tee


tty: tty.ml
	ocamlopt -I +unix unix.cmxa tty.ml -o tty


uname: uname.ml
	ocamlopt uname.ml -o uname


uptime: uptime.ml
	ocamlopt -I +unix unix.cmxa uptime.ml -o uptime
