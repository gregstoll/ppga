default: MakePngTest

MakePngTest: MakePngTest.hs MakePng.hs
	ghc -O -fglasgow-exts --make -o MakePngTest MakePngTest.hs
