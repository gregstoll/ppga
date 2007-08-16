default: MakePngTest cgiHandler cgiTester

MakePngTest: MakePngTest.hs MakePng.hs
	ghc -O -fglasgow-exts --make -o MakePngTest MakePngTest.hs

cgiHandler: cgiHandler.hs MakePng.hs
	ghc -O -fglasgow-exts --make -o cgiHandler cgiHandler.hs

cgiTester: cgiTester.hs MakePng.hs
	ghc -O -fglasgow-exts --make -o cgiTester cgiTester.hs
