drawing: src/drawing.scm
	mkdir -p build
	mit-scheme --quiet --load src/drawing.scm --eval "(main)" >build/drawing.html
	open build/drawing.html
	
