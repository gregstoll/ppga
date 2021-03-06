/*
    json.js
    2007-07-03

    Public Domain

    This file adds these methods to JavaScript:

        array.toJSONString()
        boolean.toJSONString()
        date.toJSONString()
        number.toJSONString()
        object.toJSONString()
        string.toJSONString()
            These methods produce a JSON text from a JavaScript value.
            It must not contain any cyclical references. Illegal values
            will be excluded.

            The default conversion for dates is to an ISO string. You can
            add a toJSONString method to any date object to get a different
            representation.

        string.parseJSON(filter)
            This method parses a JSON text to produce an object or
            array. It can throw a SyntaxError exception.

            The optional filter parameter is a function which can filter and
            transform the results. It receives each of the keys and values, and
            its return value is used instead of the original value. If it
            returns what it received, then structure is not modified. If it
            returns undefined then the member is deleted.

            Example:

            // Parse the text. If a key contains the string 'date' then
            // convert the value to a date.

            myData = text.parseJSON(function (key, value) {
                return key.indexOf('date') >= 0 ? new Date(value) : value;
            });

    It is expected that these methods will formally become part of the
    JavaScript Programming Language in the Fourth Edition of the
    ECMAScript standard in 2008.

    This file will break programs with improper for..in loops. See
    http://yuiblog.com/blog/2006/09/26/for-in-intrigue/

    This is a reference implementation. You are free to copy, modify, or
    redistribute.

    Use your own copy. It is extremely unwise to load untrusted third party
    code into your pages.
*/

/*jslint evil: true */

// Augment the basic prototypes if they have not already been augmented.

if (!Object.toJSONString) {

    Array.toJSONString = function (arr) {
        var a = [],     // The array holding the member texts.
            i,          // Loop counter.
            l = arr.length,
            v;          // The value to be stringified.


// For each value in this array...

        for (i = 0; i < l; i += 1) {
            v = arr[i];
            switch (typeof v) {
            case 'object':

// Serialize a JavaScript object value. Ignore objects thats lack the
// toJSONString method. Due to a specification error in ECMAScript,
// typeof null is 'object', so watch out for that case.

                if (v) {
                    //if (typeof v.toJSONString === 'function') {
                        if (v instanceof Array) {
                            a.push(Array.toJSONString(v));
                        } else {
                            a.push(Object.toJSONString(v));
                        }
                    //}
                } else {
                    a.push('null');
                }
                break;

            case 'string':
                a.push(String.toJSONString(v));
                break;
            case 'number':
                a.push(Number.toJSONString(v));
                break;
            case 'boolean':
                a.push(Boolean.toJSONString(v));
                break;

// Values without a JSON representation are ignored.

            }
        }

// Join all of the member texts together and wrap them in brackets.

        return '[' + a.join(',') + ']';
    };


    Boolean.toJSONString = function (v) {
        return String(v);
    };


    Date.toJSONString = function (v) {

// Ultimately, this method will be equivalent to the date.toISOString method.

        function f(n) {

// Format integers to have at least two digits.

            return n < 10 ? '0' + n : n;
        }

        return '"' + v.getFullYear() + '-' +
                f(v.getMonth() + 1) + '-' +
                f(v.getDate()) + 'T' +
                f(v.getHours()) + ':' +
                f(v.getMinutes()) + ':' +
                f(v.getSeconds()) + '"';
    };


    Number.toJSONString = function (v) {

// JSON numbers must be finite. Encode non-finite numbers as null.

        return isFinite(v) ? String(v) : 'null';
    };


    Object.toJSONString = function (o) {
        var a = [],     // The array holding the member texts.
            k,          // The current key.
            v;          // The current value.

// Iterate through all of the keys in the object, ignoring the proto chain.

        for (k in o) {
            if (o.hasOwnProperty(k)) {
                v = o[k];
                switch (typeof v) {
                case 'object':

// Serialize a JavaScript object value. Ignore objects that lack the
// toJSONString method. Due to a specification error in ECMAScript,
// typeof null is 'object', so watch out for that case.

                    if (v) {
                        //if (typeof v.toJSONString === 'function') {
                            // TODO - questionable - what if k isn't a String?
                            //a.push(k.toJSONString() + ':' + v.toJSONString());
                            if (v instanceof Array) {
                                a.push(String.toJSONString(k) + ':' + Array.toJSONString(v));
                            } else {
                                a.push(String.toJSONString(k) + ':' + Object.toJSONString(v));
                            }
                        //}
                    } else {
                        a.push(String.toJSONString(k) + ':null');
                    }
                    break;

                case 'string':
                    a.push(String.toJSONString(k) + ':' + String.toJSONString(v));
                    break;
                case 'number':
                    a.push(String.toJSONString(k) + ':' + Number.toJSONString(v));
                    break;
                case 'boolean':
                    a.push(String.toJSONString(k) + ':' + Boolean.toJSONString(v));
                    break;

// Values without a JSON representation are ignored.

                }
            }
        }

// Join all of the member texts together and wrap them in braces.

        return '{' + a.join(',') + '}';
    };


    (function (s) {

// Augment String.prototype. We do this in an immediate anonymous function to
// avoid defining global variables.

// m is a table of character substitutions.

        var m = {
            '\b': '\\b',
            '\t': '\\t',
            '\n': '\\n',
            '\f': '\\f',
            '\r': '\\r',
            '"' : '\\"',
            '\\': '\\\\'
        };


        s.parseJSON = function (filter, str) {
            var j;

            function walk(k, v) {
                var i;
                if (v && typeof v === 'object') {
                    for (i in v) {
                        if (v.hasOwnProperty(i)) {
                            v[i] = walk(i, v[i]);
                        }
                    }
                }
                return filter(k, v);
            }


// Parsing happens in three stages. In the first stage, we run the text against
// a regular expression which looks for non-JSON characters. We are especially
// concerned with '()' and 'new' because they can cause invocation, and '='
// because it can cause mutation. But just to be safe, we will reject all
// unexpected characters.

// We split the first stage into 3 regexp operations in order to work around
// crippling deficiencies in Safari's regexp engine. First we replace all
// backslash pairs with '@' (a non-JSON character). Second we delete all of
// the string literals. Third, we look to see if only JSON characters
// remain. If so, then the text is safe for eval.

            if (/^[,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t]+$/.test(str.
                    replace(/\\./g, '@').
                    replace(/"[^"\\\n\r]*"/g, ''))) {

// In the second stage we use the eval function to compile the text into a
// JavaScript structure. The '{' operator is subject to a syntactic ambiguity
// in JavaScript: it can begin a block or an object literal. We wrap the text
// in parens to eliminate the ambiguity.

                j = eval('(' + str + ')');

// In the optional third stage, we recursively walk the new structure, passing
// each name/value pair to a filter function for possible transformation.

                if (typeof filter === 'function') {
                    j = walk('', j);
                }
                return j;
            }

// If the text is not JSON parseable, then a SyntaxError is thrown.

            throw new SyntaxError('parseJSON');
        };


        s.toJSONString = function (str) {

// If the string contains no control characters, no quote characters, and no
// backslash characters, then we can simply slap some quotes around it.
// Otherwise we must also replace the offending characters with safe
// sequences.

            if (/["\\\x00-\x1f]/.test(str)) {
                return '"' + str.replace(/([\x00-\x1f\\"])/g, function (a, b) {
                    var c = m[b];
                    if (c) {
                        return c;
                    }
                    c = b.charCodeAt();
                    return '\\u00' +
                        Math.floor(c / 16).toString(16) +
                        (c % 16).toString(16);
                }) + '"';
            }
            return '\'' + str + '\'';
        };
    })(String);
}
