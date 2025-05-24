gail: *.lisp *.asd
	sbcl --eval "(asdf:make :gail)"

clean:
	rm -rf *~ gail
