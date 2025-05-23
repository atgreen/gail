all:
	sbcl --load gail.lisp --eval "(sb-ext:save-lisp-and-die \"gail\" :executable t :toplevel #'gail::main)"

clean:
	rm -rf *~ gail
