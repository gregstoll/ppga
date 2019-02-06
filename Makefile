default: MakePngTest cgiHandler cgiTester PngTest writepng

MakePngTest: MakePngTest.hs MakePng.hs
	ghc -O -fglasgow-exts --make -o MakePngTest MakePngTest.hs

cgiHandler: cgiHandler.hs MakePng.hs
	ghc -O -fglasgow-exts -fth --make -o cgiHandler cgiHandler.hs

cgiTester: cgiTester.hs MakePng.hs
	ghc -O -fglasgow-exts -fth --make -o cgiTester cgiTester.hs

PngTest: Png.hs PngTest.hs
	ghc -O -fglasgow-exts -fth --make -o PngTest PngTest.hs

writepng.o: writepng.cpp
	g++ -c -O2 -Wall -o writepng.o writepng.cpp

writepng: writepng.o
	g++ -O2 -Wall -lpng16 -lm -o writepng writepng.o

clean:
	rm -f writepng.o writepng
