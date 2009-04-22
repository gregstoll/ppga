//<![CDATA[

String.prototype.strip = function() {
    var i;
    for (i = 0; i < this.length && (this.charAt(i) == ' '); ++i);
    return this.slice(i);
};
var ppga;
if (!ppga) ppga = {};
ppga.Class = {
    // When making a random function, the probability that we automatically
    // select one with zero children increases by this much every time
    // (to make sure the trees don't go on forever)
    ZERO_CHILD_PROB : 0.07,
    MUTATION_PROB: 0.15,
    // The number of zero child nodes (at the beginning of fnInfo)
    NUM_ZERO_NODES : 3,
    // Number of images in a generation (ids starting at 'img1')
    NUM_IMAGES : 20,
    // Default image size
    DEFAULT_IMAGE_SIZE: 140,
    // Map of id to current function
    curFns : {},
    selectedFns : {},
    numberImagesToLoad : 0,
    detailFn : {},
    fnInfo : [
       {'type': 'num', 'children': 0, 'aux': 'val'},
       {'type': 'x', 'children': 0},
       {'type': 'y', 'children': 0},
       {'type': 'atan', 'children': 1, 'needsMap' : true},
       {'type': 'abs', 'children': 1},
       {'type': 'cos', 'children': 1},
       {'type': 'exp', 'children': 1, 'needsMap': true},
       {'type': 'log', 'children': 1, 'needsMap': true},
       {'type': 'neg', 'children': 1},
       {'type': 'rd', 'children': 1},
       {'type': 'ru', 'children': 1},
       {'type': 'sin', 'children': 1},
       {'type': 'add', 'children': 2, 'needsMap': true},
       {'type': 'div', 'children': 2, 'needsMap': true},
       {'type': 'mul', 'children': 2, 'needsMap': true},
       {'type': 'sub', 'children': 2, 'needsMap': true},
       {'type': 'ccrgb', 'children': 3},
       {'type': 'cchsl', 'children': 3}],
    // http://javascript.crockford.com/memory/leak.html
    purge: function(d) {
        var a = d.attributes, i, l, n;
        if (a) {
            l = a.length;
            for (i = 0; i < l; i += 1) {
                n = a[i].name;
                if (typeof d[n] === 'function') {
                    d[n] = null;
                }
            }
        }
        a = d.childNodes;
        if (a) {
            l = a.length;
            for (i = 0; i < l; i += 1) {
                purge(d.childNodes[i]);
            }
        }
    },

    getFnInfo: function(type) {
        for (var i = 0; i < this.fnInfo.length; ++i) {
            if (this.fnInfo[i]['type'] == type) {
                return this.fnInfo[i];
            }
        }
        return {};
    },
    
    setLoading: function(loading) {
        if (loading) {
            $('#status').show();
        } else {
            $('#status').hide();
        }
    },

    resetImagesToLoad: function() {
        this.numberImagesToLoad = 0;
        this.setLoading(false);
    },

    incImagesToLoad: function() {
        if (this.numberImagesToLoad == 0) {
            this.setLoading(true);
        }
        this.numberImagesToLoad++;
    },

    decImagesToLoad: function() {
        this.numberImagesToLoad--;
        if (this.numberImagesToLoad == 0) {
            this.setLoading(false);
        }
    },
    makeRandomMapping: function(curInfo) {
        curInfo['m'] = (Math.random() < .5) ? 'c' : 'w';
    },

    getNumberOfNodes: function(fn) {
        var total = 1;
        if ('ch' in fn) {
            for (var i = 0; i < fn['ch'].length; ++i) {
                total += this.getNumberOfNodes(fn['ch'][i]);
            }
        }
        return total;
    },

    getNodeFromNumber: function(fn, num) {
        if (num == 0) {
            return fn;
        }
        num -= 1;
        for (i = 0; i < fn['ch'].length; ++i) {
            var numChildNodes = this.getNumberOfNodes(fn['ch'][i]);
            if (num < numChildNodes) {
                return this.getNodeFromNumber(fn['ch'][i], num);
            } else {
                num -= numChildNodes;
            }
        }
    },
    splice: function(target, num, source) {
        if (num == 0) {
            return source;
        }
        num -= 1;
        for (i = 0; i < target['ch'].length; ++i) {
            var numChildNodes = this.getNumberOfNodes(target['ch'][i]);
            if (num < numChildNodes) {
                target['ch'][i] = this.splice(target['ch'][i], num, source);
                break;
            } else {
                num -= numChildNodes;
            }
        }
        return target;
    },
    clone: function(myObj) {
        if(typeof(myObj) != 'object') return myObj;
        if(myObj == null) return myObj;

        var myNewObj = new Object();
        if ("length" in myObj) {
            myNewObj = new Array();
            for (var i = 0; i < myObj.length; ++i) {
                myNewObj.push(this.clone(myObj[i]));
            }
        } else {
            for(var i in myObj) {
                myNewObj[i] = this.clone(myObj[i]);
            }
        }
        return myNewObj;
    },
    mutate: function(fn, prob) {
        if (Math.random() < prob) {
            // mutate!  Make a random node and splice it in somewhere.
            var randFn = this.makeRandomFn(0.0);
            var fnSize = this.getNumberOfNodes(fn);
            return this.splice(fn, Math.floor(Math.random() * fnSize), randFn);
        } else {
            return fn;
        }
    },
    // Chooses a random node from f1 and places it in f2
    breedFns: function(f1, f2) {
        var f2size = this.getNumberOfNodes(f2);
        var source = this.getNodeFromNumber(f2, Math.floor(Math.random() * f2size));
        var f1size = this.getNumberOfNodes(f1);
        return this.mutate(this.splice(f1, Math.floor(Math.random() * f1size), source), this.MUTATION_PROB);
    },
    nextGenStep: function(fns, i) {
        var fnsSize = fns.length;
        var fn1 = this.clone(fns[Math.floor(Math.random() * fnsSize)]);
        var fn2 = this.clone(fns[Math.floor(Math.random() * fnsSize)]);
        var newFn = this.breedFns(fn1, fn2);
        var curId = 'img' + i;
        this.curFns[curId] = newFn;
        this.incImagesToLoad();
        this.setImage(newFn, this.DEFAULT_IMAGE_SIZE, this.DEFAULT_IMAGE_SIZE, curId);
        if (i < this.NUM_IMAGES) {
            setTimeout(function() {ppga.Class.nextGenStep(fns, i+1);}, 200);
        }
    },
    nextGeneration: function() {
        // TODO - use fitness functions
        var fns = [];
        for (id in this.selectedFns) {
            if (this.selectedFns[id] == true) {
                fns.push(this.clone(this.curFns[id]));
            }
        }
        imagesInfo = {}
        var fnsSize = fns.length;
        this.resetImagesToLoad();
        for (var i = 1; i <= this.NUM_IMAGES; ++i) {
            var curId = 'img' + i;
            if (this.isSelected(curId)) {
                this.selectImage(curId);
            }
        }
        this.selectedFns = {};
        this.nextGenStep(fns, 1); 
    },

    makeRandomFn: function (randomProb, ensureTopHaveColor) {
        if (Math.random() < randomProb) {
            // select zero node
            var funcIndex = Math.floor(Math.random() * this.NUM_ZERO_NODES);
        } else {
            var funcIndex;
            if (ensureTopHaveColor) {
                funcIndex = Math.floor(16 + Math.random() * 2);
            } else {
                funcIndex = Math.floor(Math.random() * this.fnInfo.length);
            }
        }
        var curInfo = this.fnInfo[funcIndex];
        var toReturn = {'t':curInfo['type']};
        if ('needsMap' in curInfo && curInfo['needsMap'])  {
            this.makeRandomMapping(toReturn);                
        }
        if ('aux' in curInfo) {
            toReturn[curInfo['aux']] = Math.random();
        }
        if (curInfo['children'] > 0) {
            toReturn['ch'] = [];
            for (var i = 0; i < curInfo['children']; ++i) {
                // Create a random child, adding the ZERO_CHILD_PROB
                // Make this a dictionary with a string key to fix weirdness
                toReturn['ch'].push(this.makeRandomFn(randomProb+this.ZERO_CHILD_PROB));
            }
        }
        return toReturn;
    },
    makeCgiURL: function(width, height, fn) {
        return pngScript+"?w="+width+"&h="+height+"&f="+Object.toJSONString(fn);
    },
    setImage: function(fn, width, height, imgId) {
        // FFV - worry about doing POST here?
        var img = document.getElementById(imgId);
        img.width = width;
        img.height = height;
        var newSrc = this.makeCgiURL(width, height, fn);
        img.src = newSrc;
        //alert("done");
    },
    makeRandomImages: function() {
        var ensureTopHaveColor = $('#newHaveColor').attr('checked');
        this.resetImagesToLoad();
        for (var i = 1; i <= this.NUM_IMAGES; ++i) {
            var curId = 'img' + i;
            var fn = this.makeRandomFn(0.0, ensureTopHaveColor);
            this.curFns[curId] = fn;
            this.incImagesToLoad();
            this.setImage(fn, this.DEFAULT_IMAGE_SIZE, this.DEFAULT_IMAGE_SIZE, curId);
        }
    },
    init: function() {
        for (var i = 1; i <= this.NUM_IMAGES; ++i) {
            var curId = 'img' + i;
            $("#" + curId).addClass('img');
        }
        $(".img").width(120).height(120);
        $(".img").click(function() {ppga.Class.selectImage(this.id);});
        $(".img").dblclick(function() {ppga.Class.details(this.id);});
        $(".img").load(function() {ppga.Class.load(this.id, true);});
        $(".img").error(function() {ppga.Class.load(this.id, false);});
        $("#detailsWidth").keyup(this.listenForEnter);
        $("#detailsHeight").keyup(this.listenForEnter);
        $("#functionStr").keyup(this.listenForEnter);
        this.makeRandomImages();
    },
    uninit: function() {
        for (var i = 1; i <= this.NUM_IMAGES; ++i) {
            var curId = 'img' + i;
            this.purge(document.getElementById(curId));
        } 
        this.purge(document.getElementById('detailsImg'));
        this.purge(document.getElementById('detailsWidth'));
        this.purge(document.getElementById('detailsHeight'));
        this.purge(document.getElementById('functionStr'));
    },
    listenForEnter: function(event) {
        var keyCode = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;
        if (keyCode == 13) {
            $("#detailsResize").click();
        }
    },
    isSelected: function(id) {
        return (id in this.selectedFns && this.selectedFns[id]);
    },
    selectImage: function(id) {
        //alert('click on id: ' + id);
        if (this.isSelected(id)) {
            this.selectedFns[id] = false;
            $("#" + id).css({'border-style': 'none', 'margin': '2px'});
        } else {
            this.selectedFns[id] = true;
            $("#" + id).css({'border-style': 'solid', 'margin': '0px'});
        }
        this.updateNextGenerationButton();
    },
    load: function(id, success) {
        this.decImagesToLoad();
    },
    scrollDetails: function() {
        window.location = '#details';
    },
    parseFriendlyString: function(str) {
        var fn = {};
        // strip spaces from the front
        str = str.strip();
        if (str.charAt(0) != 'x' && str.charAt(0) != 'y' && str.charAt(0) != '(') {
            str = str.slice(1);
            str = str.strip();
        }
        var gotParen = false;
        if (str.charAt(0) == '(') {
            gotParen = true;
            str = str.slice(1);
        }
        // extract the name and mapping (if applicable)
        var spaceIndex = str.indexOf(' ');
        var closeParenIndex = str.indexOf(')');
        if (spaceIndex == -1) {
            spaceIndex = str.length + 1;
        }
        if (closeParenIndex == -1) {
            closeParenIndex = str.length + 1;
        }
        var indexToUse = Math.min(spaceIndex, closeParenIndex);
        var type = str.substring(0, indexToUse);
        var rest = str.substring(indexToUse);
        if (type.substring(type.length-5) == '-wrap') {
            type = type.substring(0, type.length-5); 
            fn['m'] = 'w';
        } else if (type.substring(type.length-5) == '-clip') {
            type = type.substring(0, type.length-5); 
            fn['m'] = 'c';
        }
        fn['t'] = type;
        // see how many children to expect and parse them.
        var typeInfo = this.getFnInfo(type);
        var numChildren = typeInfo['children'];
        if (numChildren > 0) {
            fn['ch'] = [];
            for (var i = 0; i < numChildren; ++i) {
                var parsedChildInfo = this.parseFriendlyString(rest);
                rest = parsedChildInfo['rest'];
                fn['ch'].push(parsedChildInfo['fn']);
            }
        } else if (type == 'num') {
            rest = rest.strip();
            var spaceIndex = rest.indexOf(' ');
            var closeParenIndex = rest.indexOf(')');
            if (spaceIndex == -1) {
                spaceIndex = str.length + 1;
            }
            if (closeParenIndex == -1) {
                closeParenIndex = str.length + 1;
            }
            var indexToUse = Math.min(spaceIndex, closeParenIndex);
            var floatStr = rest.substring(0, indexToUse);
            rest = rest.substring(indexToUse);
            fn['val'] = parseFloat(floatStr);
        }
        if (gotParen) {
            rest = rest.strip();
            // This had better be a paren...
            rest = rest.substring(1);
        }
        return {'fn': fn, 'rest': rest};
    },
    friendlyString: function(fn) {
        var str = '';
        var useParen = true;
        if (fn['t'] == 'x' || fn['t'] == 'y') {
            useParen = false;
        }
        if (useParen) {
            str += '(';
        }
        str += fn['t'];
        if ('m' in fn) {
            str += '-' + ((fn['m'] == 'w') ? 'wrap' : 'clip');
        }
        var numChildren = 0;
        if ('ch' in fn) {
            numChildren = fn['ch'].length;
        }
        if (numChildren > 0) {
            str += ' ';
            for (var i = 0; i < numChildren; ++i) {
                str += this.friendlyString(fn['ch'][i]);
                if (i < numChildren - 1) {
                    str += ' ';
                }
            }
        }
        if ('val' in fn) {
            str += (' ' + fn['val']);
        }
        if (useParen) {
            str += ')';
        }
        return str;
    },
    makeResizedDetail: function(w, h) {
        var img = $('#detailsImg');
        var detailFn = this.parseFriendlyString($('#functionStr').attr('value'))['fn'];
        this.detailFn = detailFn;
        this.resetImagesToLoad();
        this.incImagesToLoad();
        img.load(function() {ppga.Class.load(this.id, true);});
        img.error(function() {ppga.Class.load(this.id, false);});
        img.attr({width: w, height: h, src: this.makeCgiURL(w, h, this.detailFn)});
    },
    loadFnIntoGen: function(fn, id) {
        this.curFns[id] = fn;
        this.resetImagesToLoad();
        this.incImagesToLoad();
        this.setImage(fn, this.DEFAULT_IMAGE_SIZE, this.DEFAULT_IMAGE_SIZE, id);
        this.cancelLoadFnIntoGen();
    },
    cancelLoadFnIntoGen: function() {
        $("body").unbind('mousedown');
        $(".img").unbind('mousedown');
        $("#selectFnImg").css({'display': 'none'});
    },
    loadIntoGen: function(fStr) {
        fStr = fStr.strip();
        if (fStr != '') {
            fn = this.parseFriendlyString(fStr)['fn'];
            $(".img").mousedown(function() {ppga.Class.loadFnIntoGen(fn, this.id);});
            $("body").mousedown(function() {ppga.Class.cancelLoadFnIntoGen();});
            $("#selectFnImg").css({'display': 'block'});
        }
    },
    details: function(id) {
        var startWidth = parseInt($('#detailsWidth').attr("value"));
        var startHeight = parseInt($('#detailsHeight').attr("value"));
        this.resetImagesToLoad();
        var detailsDiv = $('#details');
        var fn = this.curFns[id];
        this.detailFn = fn;
        $('#detailsResize').attr({value: "Resize image"});
        $('#functionStr').attr({value: this.friendlyString(fn)});
        this.incImagesToLoad();
        $('#detailsImg').load(function() {ppga.Class.load(this.id, true);});
        $('#detailsImg').error(function() {ppga.Class.load(this.id, false);});
        $('#detailsImg').attr({width: startWidth, height: startHeight, src: this.makeCgiURL(startWidth, startHeight, fn)});
        setTimeout(ppga.Class.scrollDetails, 500);
    },
    updateNextGenerationButton: function() {
        var oneSelected = false;
        for (var i = 1; i <= this.NUM_IMAGES; ++i) {
            if (this.isSelected('img' + i)) {
                oneSelected = true;
                break;
            }
        }
        var nextGenButton = document.getElementById('nextGenButton');
        if (oneSelected) {
            nextGenButton.disabled = false;
        } else {
            nextGenButton.disabled = true;
        }
    },
    randomFnTest: function() {
        var fn = this.makeRandomFn(0.0);
        $('#fnDesc').empty();
        $('#fnDesc').append(Object.toJSONString(fn) + " " + Object.toJSONString(fn).length);
        this.setImage(fn, this.DEFAULT_IMAGE_SIZE, this.DEFAULT_IMAGE_SIZE, 'testImg');
    }
};

(function() {
    $(document).ready(function() {ppga.Class.init();});
    $(document).unload(function() {ppga.Class.uninit();});
})();
//]]>
