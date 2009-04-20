#include <iostream>
//#include <stdio.h>
#include <string>
#include <map>
#include <stdlib.h>
#define PNG_SETJMP_NOT_SUPPORTED
#include "png.h"

using namespace std;

string pngOutput;
void writepng_version_info()
{
    //fprintf(stderr, "   Compiled with libpng %s; using libpng %s.\n",
                  //PNG_LIBPNG_VER_STRING, png_libpng_ver);
    //fprintf(stderr, "   Compiled with zlib %s; using zlib %s.\n",
                   // ZLIB_VERSION, zlib_version);
}

void user_write_data(png_structp png_ptr, png_bytep data, png_size_t length) {
    //cerr << "TODO - writing " << length << " bytes" << endl;
    size_t oldSize = pngOutput.size();
    pngOutput.resize(oldSize + length);
    // TODO - isn't there a better way to do this?
    memcpy(&pngOutput[0] + oldSize, data, length);
}

void user_flush_data(png_structp png_ptr) {
    // Nothing to do.
    //cerr << "TODO - flush" << endl;
}

// Adapted from code at
//  http://www.purplepixie.org/cgi/download/cgi_interface.cpp
//  Note that this doesn't support multiple keys, but that's OK
//  for our purposes.
map<string, string> parse_query_string(string input) {
    map<string, string> queryMap;
    if (input.size() <= 0) return queryMap;
    string curName;
    string curValue;
    bool name=true;
    long val=0;
    char d[5];
    for (string::iterator inputIt = input.begin(); inputIt < input.end(); ++inputIt) {
        /*if (name) {
            cname[pos]=0; cname[pos+1]=0;
        }
        else {
            cvalue[pos]=0; cvalue[pos+1]=0;
        }*/
        switch (*inputIt) {
            case '=':
                if (name) {
                    name=false;
                }
            break;
            case '&':
                queryMap[curName] = curValue;
                curName.clear();
                curValue.clear();
                name=true;
                break;
            case '\%':
                strncpy(d,&(inputIt+1)[0],2);
                val=strtol(d,(char**)NULL,16);
                if (name) curName.append(1, val);
                else curValue.append(1, val);
                inputIt += 2;
                break;
            case '+':
                if (name) curName.append(1, ' ');
                else curValue.append(1, ' ');
                break;
            default:
                if (name) curName.append(1, *inputIt);
                else curValue.append(1, *inputIt);
        }
    }
    if (curName.size() > 0) {
        queryMap[curName] = curValue;
    }
    return queryMap;
}

int main(int argc, char** argv) {
    //writepng_version_info();
    int errorCode = 0;
    png_structp png_ptr = NULL;
    png_infop info_ptr = NULL;
    png_color* color_data = NULL;
    try {
        int width = 35;
        int height = 40;
        png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
        if (png_ptr == NULL) {
            throw "couldn't initialize png_ptr";
        }
        info_ptr = png_create_info_struct(png_ptr);
        if (info_ptr == NULL) {
            throw "couldn't initialize info_ptr";
        }
        // We want to write to a string, so set that up.
        png_set_write_fn(png_ptr, png_get_io_ptr(png_ptr), user_write_data, user_flush_data);

        // Set up some fake data.
        color_data = new png_color[width*height];
        for (int y = 0 ; y < height; ++y) {
            for (int x = 0 ; x < width; ++x) {
                color_data[y*width+x].red = y*2;
                color_data[y*width+x].green = 0;
                color_data[y*width+x].blue = 0;
            }
        }

        png_set_IHDR(png_ptr, info_ptr, width, height, 8, PNG_COLOR_TYPE_RGB, PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);

        // Now set up the row pointers.
        png_byte** color_rows = new png_byte*[height];
        for (int i = 0; i < height; ++i) {
            color_rows[i] = reinterpret_cast<png_byte*>(&color_data[width * i]);
        }
        png_set_rows(png_ptr, info_ptr, color_rows);
        png_write_png(png_ptr, info_ptr, PNG_TRANSFORM_IDENTITY, NULL);
        delete[] color_rows;
    } catch (char* err) {
        cerr << "catching error " << err << endl;
        errorCode = 1;
    }
    printf("Content-type: image/png\n\n");
    for (unsigned int i = 0; i < pngOutput.size(); ++i) {
        printf("%c", pngOutput[i]);
    }
    //cerr << "about to destroy" << endl;
    if (png_ptr && info_ptr) {
        png_destroy_write_struct(&png_ptr, &info_ptr);
    }
    delete[] color_data;
    return errorCode;
}
