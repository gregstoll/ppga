<!-- This example is from the book _JavaScript: The Definitive Guide_.     -->
<!-- Written by David Flanagan.  Copyright (c) 1996 O'Reilly & Associates. -->
<!-- This example is provided WITHOUT WARRANTY either expressed or implied.-->
<!-- You may study, use, modify, and distribute it for any purpose.        -->
// This is a long string in XBM image format.  It defines an image.
// This is an ASCII format, which means we can easily manipulate it 
// in JavaScript, but also means that it is not compact.  This is only
// a 22x22 pixel image.  The real power of this technique comes, of course
// when we start generating XBM data dynamically at run time instead of
// using a static string as we do here.
var image_text = 
"#define plaid_width 22\n" +
"#define plaid_height 22\n" +
"#define plaid_x_hot -1\n" +
"#define plaid_y_hot -1\n" +
"static char plaid_bits[] = {\n" +
"  0x75, 0xfd, 0x3f, 0xaa, 0xfa, 0x3e, 0x75, 0xfd, 0x3f, 0xaa, 0xfa, 0x3e,\n" +
"  0x75, 0xfd, 0x3f, 0xff, 0x57, 0x15, 0x75, 0xfd, 0x3f, 0xaa, 0xfa, 0x3e,\n" +
"  0x75, 0xfd, 0x3f, 0xaa, 0xfa, 0x3e, 0x75, 0xfd, 0x3f, 0x20, 0xa8, 0x2b,\n" +
"  0x20, 0x50, 0x15, 0x20, 0xa8, 0x2b, 0x20, 0x50, 0x15, 0x20, 0xa8, 0x2b,\n" +
"  0xff, 0xff, 0x3f, 0x20, 0xa8, 0x2b, 0x20, 0x50, 0x15, 0x20, 0xa8, 0x2b,\n" +
"  0x20, 0x50, 0x15, 0x20, 0xa8, 0x2b};\n";

// Here we create a new window, open the document, specifying a MIME type of
// image/xbm, and then output the image text.  The window will display
// the XBM data we give it.
/*win1 = window.open("", "win1", "width=100,height=100,resizable");
var d = win1.document;
d.open('image/xbm');
d.write(image_text);
d.close();*/

// There are also a couple of other ways to use XBM image data that do not
// involve specifying a MIME type when opening the document.  Here we 
// create a new window, and then use a javascript: URL as the SRC of an
// inline <IMG>.  This is an XBM image embedded in a text/html document,
// so we can display text, anchors, etc.
/*win2 = window.open("", "win2", "width=100,height=100,resizable");
var d = win2.document;
d.open();
d.write('<B>Plaid:</B><BR>');
d.write('<A HREF="javascript:self.close();">');
d.write('<IMG SRC="javascript:opener.image_text" WIDTH=22 HEIGHT=22>');
d.write('</A>');
d.close();*/

// We can also use the javascript: URL with the BACKGROUND tag of the
// <BODY> tag.  XBM is a black-on-white image format, but note how the
// BGCOLOR tag can replace the white background.
/*win3 = window.open("", "win3", "width=100,height=100,resizable");
var d = win3.document;
d.open();
d.write('<BODY BACKGROUND="javascript:opener.image_text" BGCOLOR="red">');
d.close();*/
