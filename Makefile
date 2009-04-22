default: MakePngTest cgiHandler cgiTester PngTest writepng

MakePngTest: MakePngTest.hs MakePng.hs
	ghc -O -fglasgow-exts --make -o MakePngTest MakePngTest.hs

cgiHandler: cgiHandler.hs MakePng.hs
	ghc -O -fglasgow-exts -fth --make -o cgiHandler cgiHandler.hs

cgiTester: cgiTester.hs MakePng.hs
	ghc -O -fglasgow-exts -fth --make -o cgiTester cgiTester.hs

PngTest: Png.hs PngTest.hs
	ghc -O -fglasgow-exts -fth --make -o PngTest PngTest.hs

writepng: writepng.cpp tinyjson.hpp
	g++ -O -Wall -o writepng -lpng writepng.cpp
