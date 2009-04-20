#include <iostream>
//#include <stdio.h>
#include <string>
#include <map>
#include <stdlib.h>
#define PNG_SETJMP_NOT_SUPPORTED
#include "png.h"
#include "tinyjson.hpp"
#include <math.h>

using namespace std;

string pngOutput;
typedef struct double_color {
    double red;
    double green;
    double blue;
} double_color;
void writepng_version_info()
{
    //fprintf(stderr, "   Compiled with libpng %s; using libpng %s.\n",
                  //PNG_LIBPNG_VER_STRING, png_libpng_ver);
    //fprintf(stderr, "   Compiled with zlib %s; using zlib %s.\n",
                   // ZLIB_VERSION, zlib_version);
}

void user_write_data(png_structp png_ptr, png_bytep data, png_size_t length) {
    size_t oldSize = pngOutput.size();
    pngOutput.resize(oldSize + length);
    // TODO - isn't there a better way to do this?
    memcpy(&pngOutput[0] + oldSize, data, length);
}

void user_flush_data(png_structp png_ptr) {
    // Nothing to do.
}


double render_function_x(double x, double y, double aux) {
    return x;
}
double render_function_y(double x, double y, double aux) {
    return y;
}
double render_function_num(double x, double y, double aux) {
    return aux;
}

double render_function_atan(double arg1) {
    return atan(arg1);
}
double render_function_abs(double arg1) {
    return abs(arg1);
}
double render_function_cos(double arg1) {
    return cos(arg1);
}
double render_function_exp(double arg1) {
    return exp(arg1);
}
double render_function_log(double arg1) {
    if (arg1 <= 0) {
        return 0;
    } else {
        return log(arg1);
    }
}
double render_function_neg(double arg1) {
    return -1.0 * arg1;
}
double render_function_rd(double arg1) {
    return floor(arg1);
}
double render_function_ru(double arg1) {
    return ceil(arg1);
}
double render_function_sin(double arg1) {
    return ceil(arg1);
}

double render_function_add(double arg1, double arg2) {
    return arg1 + arg2;
}
double render_function_div(double arg1, double arg2) {
    if (arg2 == 0) {
        return 0;
    } else {
        return arg1 / arg2;
    }
}
double render_function_mul(double arg1, double arg2) {
    return arg1 * arg2;
}
double render_function_sub(double arg1, double arg2) {
    return arg1 - arg2;
}


typedef double (*ZeroArgRenderFn)(double x, double y, double aux);
typedef double (*OneArgRenderFn)(double arg1);
typedef double (*TwoArgRenderFn)(double arg1, double arg2);

inline double finalHslToRgb(double tc, double q, double p) {
    if (tc < 1.0/6.0) {
        return p + ((q - p) * 6.0 * tc);
    } else if (tc < 1.0/2.0) {
        return q;
    } else if (tc < 2.0/3.0) {
        return p + ((q - p) * ((2.0/3.0) - tc) * 6.0);
    } else {
        return p;
    }
}

typedef enum mapping_mode_enum {
    None,
    Clip,
    Wrap
} mapping_mode;

typedef double (*WrappingFn)(double val);
template <mapping_mode M>
struct DoWrap {
    static double doWrap(double val) {
        if (M == None) {
            return val;
        } else if (M == Clip) {
            if (val > 1) {
                return 1;
            } else if (val < -1) {
                return -1;
            }
            return val;
        } else {
            if (val == 1) {
                return 1;
            }
            return fmod(val + 1, 2) - 1;;
        }
    }
};
/*template <>
class DoWrap<Clip> {
    static double doWrap(double val) { 
   }
};
template <>
class DoWrap<Wrap> {
    static double doWrap(double val) { 
   }
};*/

// FFV - we could optimize by only calculating r, g, or b depending on what
// the higher functions need.
void render_function(json::grammar<char>::variant funcJson, double_color* output, int width, int height, bool topLevel) {
    double_color* color_data_temp1 = NULL;
    double_color* color_data_temp2 = NULL;
    double_color* color_data_temp3 = NULL;

    // This should always be true
    if (funcJson->type() == typeid(json::grammar<char>::object)) {
        json::grammar<char>::object const &funcObj = boost::any_cast< json::grammar<char>::object >(*funcJson);
        // Get the type of the function.
        json::grammar<char>::object::const_iterator objectIt = funcObj.find("t");
        if (objectIt == funcObj.end()) {
            throw "Function has no t member!";
        }
        string t = boost::any_cast< string >(*(objectIt->second));
        // FFV - we could do some crazy map here or something, but come on.
        // Or we could have a function for each type, once you evaluate its
        // arguments.
        // Do the zero-arg ones
        if (t == "x" || t == "y" || t == "num") {
            ZeroArgRenderFn renderFn = NULL;
            if (t == "x") {
                renderFn = render_function_x;
            } else if (t == "y") {
                renderFn = render_function_y;
            } else if (t == "num") {
                renderFn = render_function_num;
            }
            double aux = 0.0;
            if (t == "num") {
                objectIt = funcObj.find("val");
                if (objectIt == funcObj.end()) {
                    throw "Function num has no val member!";
                }
                aux = boost::any_cast<double>(*objectIt);
            }
            double deltaX = 2.0 / width;
            double deltaY = 2.0 / height;
            double x = -1.0;
            double y = 1.0;
            double_color* tempPtr = output;
            for (int yIt = 0; yIt < height; ++yIt, y -= deltaY) {
                for (int xIt = 0; xIt < width; ++xIt, x += deltaX) {
                    tempPtr->red = tempPtr->green = tempPtr->blue = renderFn(x, y, aux);
                    ++tempPtr;
                }
            }
        } else {
            // Get the argument function.
            objectIt = funcObj.find("ch");
            if (objectIt == funcObj.end()) {
                throw "arg function has no ch member!";
            }
            json::grammar<char>::array const &argArray = boost::any_cast< json::grammar<char>::array >(*(objectIt->second));
 
            if (t == "atan" || t == "abs" || t == "cos" || t == "exp" || t == "log" || t == "neg" || t == "rd" || t == "ru" || t == "sin") {
                // Do the one-arg ones.
                // First, we have to recur.
                color_data_temp1 = new double_color[width * height];
                json::grammar<char>::variant arg0 = argArray[0];
                render_function(arg0, color_data_temp1, width, height, false);
                OneArgRenderFn renderFn = NULL;
                if (t == "atan") {
                    renderFn = render_function_atan;
                } else if (t == "abs") {
                    renderFn = render_function_abs;
                } else if (t == "cos") {
                    renderFn = render_function_cos;
                } else if (t == "exp") {
                    renderFn = render_function_exp;
                } else if (t == "log") {
                    renderFn = render_function_log;
                } else if (t == "neg") {
                    renderFn = render_function_neg;
                } else if (t == "rd") {
                    renderFn = render_function_rd;
                } else if (t == "ru") {
                    renderFn = render_function_ru;
                } else if (t == "sin") {
                    renderFn = render_function_sin;
                }
                // Look at the mapping strategy, if present.
                // I thought this would make things faster, but now I
                // think I'm just fooling myself.
                WrappingFn wrappingFn = DoWrap<None>::doWrap;
                objectIt = funcObj.find("m");
                if (objectIt != funcObj.end()) {
                    if (boost::any_cast<string>(*(objectIt->second)) == "c") {
                        wrappingFn = DoWrap<Clip>::doWrap;
                    } else {
                        wrappingFn = DoWrap<Wrap>::doWrap;
                    }
                }

                double_color* tempPtr = output;
                double_color* arg1Ptr = color_data_temp1;
                for (int yIt = 0; yIt < height; ++yIt) {
                    for (int xIt = 0; xIt < width; ++xIt) {
                        tempPtr->red = wrappingFn(renderFn(arg1Ptr->red));
                        tempPtr->green = wrappingFn(renderFn(arg1Ptr->green));
                        tempPtr->blue = wrappingFn(renderFn(arg1Ptr->blue));
                        ++tempPtr;
                        ++arg1Ptr;
                    }
                }

                delete[] color_data_temp1;
            } else if (t == "add" || t == "div" || t == "mul" || t == "sub") {
                // Do the two-arg ones.
                // First, we have to recur.
                color_data_temp1 = new double_color[width * height];
                color_data_temp2 = new double_color[width * height];
                json::grammar<char>::variant arg0 = argArray[0];
                json::grammar<char>::variant arg1 = argArray[1];
                render_function(arg0, color_data_temp1, width, height, false);
                render_function(arg1, color_data_temp2, width, height, false);
                TwoArgRenderFn renderFn = NULL;
                if (t == "add") {
                    renderFn = render_function_add;
                } else if (t == "div") {
                    renderFn = render_function_div;
                } else if (t == "mul") {
                    renderFn = render_function_mul;
                } else if (t == "sub") {
                    renderFn = render_function_sub;
                }

                // Look at the mapping strategy, if present.
                // I thought this would make things faster, but now I
                // think I'm just fooling myself.
                WrappingFn wrappingFn = DoWrap<None>::doWrap;
                objectIt = funcObj.find("m");
                if (objectIt != funcObj.end()) {
                    if (boost::any_cast<string>(*(objectIt->second)) == "c") {
                        wrappingFn = DoWrap<Clip>::doWrap;
                    } else {
                        wrappingFn = DoWrap<Wrap>::doWrap;
                    }
                }

                double_color* tempPtr = output;
                double_color* arg1Ptr = color_data_temp1;
                double_color* arg2Ptr = color_data_temp2;
                for (int yIt = 0; yIt < height; ++yIt) {
                    for (int xIt = 0; xIt < width; ++xIt) {
                        tempPtr->red = wrappingFn(renderFn(arg1Ptr->red, arg2Ptr->red));
                        tempPtr->green = wrappingFn(renderFn(arg1Ptr->green, arg2Ptr->green));
                        tempPtr->blue = wrappingFn(renderFn(arg1Ptr->blue, arg2Ptr->blue));
                        ++tempPtr;
                        ++arg1Ptr;
                        ++arg2Ptr;
                    }
                }

                delete[] color_data_temp1;
                delete[] color_data_temp2;
            } else if (t == "ccrgb" || t == "cchsl") {
                // Do the three-arg ones.
                // First, we have to recur.
                color_data_temp1 = new double_color[width * height];
                color_data_temp2 = new double_color[width * height];
                color_data_temp3 = new double_color[width * height];
                json::grammar<char>::variant arg0 = argArray[0];
                json::grammar<char>::variant arg1 = argArray[1];
                json::grammar<char>::variant arg2 = argArray[2];
                render_function(arg0, color_data_temp1, width, height, false);
                render_function(arg1, color_data_temp2, width, height, false);
                render_function(arg2, color_data_temp3, width, height, false);

                // There are only two three-arg functions, and they're a bit
                // complicated, so roll our own.
                if (t == "ccrgb") {
                    double_color* tempPtr = output;
                    double_color* arg1Ptr = color_data_temp1;
                    double_color* arg2Ptr = color_data_temp2;
                    double_color* arg3Ptr = color_data_temp3;
                    for (int yIt = 0; yIt < height; ++yIt) {
                        for (int xIt = 0; xIt < width; ++xIt) {
                            tempPtr->red = arg1Ptr->red;
                            tempPtr->green = arg2Ptr->green;
                            tempPtr->blue = arg3Ptr->blue;
                            ++tempPtr;
                            ++arg1Ptr;
                            ++arg2Ptr;
                            ++arg3Ptr;
                        }
                    }
                } else if (t == "cchsl") {
                    double_color* tempPtr = output;
                    double_color* arg1Ptr = color_data_temp1;
                    double_color* arg2Ptr = color_data_temp2;
                    double_color* arg3Ptr = color_data_temp3;
                    for (int yIt = 0; yIt < height; ++yIt) {
                        for (int xIt = 0; xIt < width; ++xIt) {
                            // take h, s, l as grayscale
                            double hOrig = .3 * arg1Ptr->red + .59 * arg1Ptr->green + .11 * arg1Ptr->blue;
                            double sOrig = .3 * arg2Ptr->red + .59 * arg2Ptr->green + .11 * arg2Ptr->blue;
                            double lOrig = .3 * arg3Ptr->red + .59 * arg3Ptr->green + .11 * arg3Ptr->blue;
                            // Scale h, s and l to be 0 to 1
                            double h = (hOrig + 1.0) / 2.0;
                            double s = (sOrig + 1.0) / 2.0;
                            double l = (lOrig + 1.0) / 2.0;
                            if (s == 0.0) {
                                tempPtr->red = l * 2.0 - 1;
                                tempPtr->green = l * 2.0 - 1;
                                tempPtr->blue = l * 2.0 - 1; 
                            } else {
                                double q;
                                if (l < .5) {
                                    q = l * (1.0 + s);
                                } else {
                                    q = l + s - (l * s);
                                }
                                double p = 2.0 * l - q;
                                double tr = h + 1.0/3.0;
                                double tg = h;
                                double tb = h - 1.0/3.0;
                                if (tr < 0) {
                                    tr += 1;
                                } else if (tr > 1) {
                                    tr -= 1;
                                }
                                if (tg < 0) {
                                    tg += 1;
                                } else if (tg > 1) {
                                    tg -= 1;
                                }
                                if (tb < 0) {
                                    tb += 1;
                                } else if (tb > 1) {
                                    tb -= 1;
                                }
                                double cr = finalHslToRgb(tr, q, p);
                                double cg = finalHslToRgb(tg, q, p);
                                double cb = finalHslToRgb(tb, q, p);
                                tempPtr->red = cr * 2.0 - 1.0;
                                tempPtr->green = cg * 2.0 - 1.0;
                                tempPtr->blue = cb * 2.0 - 1.0;
                            }

                            ++tempPtr;
                            ++arg1Ptr;
                            ++arg2Ptr;
                            ++arg3Ptr;
                        }
                    }
                }


                delete[] color_data_temp1;
                delete[] color_data_temp2;
                delete[] color_data_temp3;
            } else {
                throw "Unknown function";
            }


        }
    }

    if (topLevel) {
        // Coerce to [-1,1]
        // FFV - do we need this?
        double_color* tempPtr = output;
        for (int yIt = 0; yIt < height; ++yIt) {
            for (int xIt = 0; xIt < width; ++xIt) {
                tempPtr->red = DoWrap<Clip>::doWrap(tempPtr->red);
                tempPtr->green = DoWrap<Clip>::doWrap(tempPtr->green);
                tempPtr->blue = DoWrap<Clip>::doWrap(tempPtr->blue);
                ++tempPtr;
            }
        }
    }


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
        char* envString = getenv("QUERY_STRING");
        if (!envString) {
            throw "No arguments passed!";
        }
        map<string, string> params = parse_query_string(string(envString));
        map<string, string>::iterator it = params.find("f");
        if (it == params.end()) {
            throw "No f argument passed!";
        }
        string funcString = it->second;
        it = params.find("w");
        if (it == params.end()) {
            throw "No w argument passed!";
        }
        int width = strtol(&(it->second[0]), NULL, 10);
        it = params.find("h");
        if (it == params.end()) {
            throw "No h argument passed!";
        }
        int height = strtol(&(it->second[0]), NULL, 10);
        // We can guess about how big our output will be.
        pngOutput.reserve(width*height*24 + 50);

        // Parse the JSON.
        json::grammar<char>::variant funcJson = json::parse(funcString.begin(), funcString.end());
        if (funcJson->type() != typeid(json::grammar<char>::object)) {
            // Bad format!
            throw "Bad JSON format!";
        }
        //
        // Set up some fake data.
        double_color* double_data = new double_color[width*height];
        png_color* color_data = new png_color[width*height];
        render_function(funcJson, double_data, width, height, true);
        // convert double_data to a png_color*
        const double_color * tempDouble = double_data;
        png_color * tempColor = color_data;
        for (int y = 0; y < height; ++y) {
            for (int x = 0; x < width; ++x) {
                tempColor->red = static_cast<char>((tempDouble->red + 1.0) * (255.0/2.0));
                tempColor->green = static_cast<char>((tempDouble->green + 1.0) * (255.0/2.0));
                tempColor->blue = static_cast<char>((tempDouble->blue + 1.0) * (255.0/2.0));
                ++tempDouble;
                ++tempColor;
            }
        }
        delete[] double_data;


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

        png_set_IHDR(png_ptr, info_ptr, width, height, 8, PNG_COLOR_TYPE_RGB, PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);

        // Now set up the row pointers.
        png_byte** color_rows = new png_byte*[height];
        for (int i = 0; i < height; ++i) {
            color_rows[i] = reinterpret_cast<png_byte*>(&color_data[width * i]);
        }
        png_set_rows(png_ptr, info_ptr, color_rows);
        png_write_png(png_ptr, info_ptr, PNG_TRANSFORM_IDENTITY, NULL);
        delete[] color_rows;
    } catch (char const* err) {
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
