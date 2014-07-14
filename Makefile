all:
	corebuild -no-links -lib str -pkg cohttp.async,yojson mathjean.native
	mv _build/mathjean.native ancestors

clean:
	corebuild -clean
