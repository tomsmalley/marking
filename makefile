docs:
	nix-build --attr ghcjs.marking
	mkdir docs
	echo '<!doctype html><html><head></head><body></body><script language="javascript" src="all.js" defer></script></html>' > docs/index.html
	nix-shell -p closurecompiler --run 'closure-compiler \
		--js_output_file docs/all.js \
		--externs=./result/bin/marking.jsexe/all.js.externs \
		./result/bin/marking.jsexe/all.js \
		--jscomp_off=checkVars \
		-O ADVANCED -W QUIET'

