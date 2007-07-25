default: MakePngTest

MakePngTest: MakePngTest.hs MakePng.hs
	ghc -O --make -o MakePngTest MakePngTest.hs
