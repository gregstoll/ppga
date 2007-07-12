//<![CDATA[

var ppga;
if (!ppga) ppga = {};
ppga.Class = {
    // When making a random function, the probability that we automatically
    // select one with zero children increases by this much every time
    // (to make sure the trees don't go on forever)
    ZERO_CHILD_PROB : 0.07,
    // The number of zero child nodes (at the beginning of fnInfo)
    NUM_ZERO_NODES : 3,
    // Number of images in a generation (ids starting at 'img1')
    NUM_IMAGES : 20,
    // Map of id to current function
    curFns : {},
    selectedFns : {},
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
       {'type': 'cc', 'children': 3}],

    makeRandomMapping: function(curInfo) {
        curInfo['m'] = (Math.random() < .5) ? 'c' : 'w';
    },

    getNumberOfNodes: function(fn) {
        var total = 1;
        if ('ch' in fn) {
            for (var i = 0; i < fn['ch'].length; ++i) {
                total += this.getNumberOfNodes(fn['ch']);
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

    // Chooses a random node from f1 and places it in f2
    // TODO - mutate
    breedFns: function(f1, f2) {
        var f2size = this.getNumberOfNodes(f2);
        var source = this.getNodeFromNumber(f2, Math.floor(Math.random() * f2size));
        var f1size = this.getNumberOfNodes(f1);
        return this.splice(f1, Math.floor(Math.random() * f1size), source);
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
        for (var i = 1; i <= this.NUM_IMAGES; ++i) {
            // TODO - use setTimeout() here?
            var fn1 = this.clone(fns[Math.floor(Math.random() * fnsSize)]);
            var fn2 = this.clone(fns[Math.floor(Math.random() * fnsSize)]);
            var newFn = this.breedFns(fn1, fn2);
            var curId = 'img' + i;
            if (this.isSelected(curId)) {
                //alert('id ' + curId + ' is selected');
                this.selectImage(curId);
            }
            this.curFns[curId] = newFn;
            this.setImage(newFn, 0, 0, curId);
        }
        this.selectedFns = {};
    },

    makeRandomFn: function (randomProb) {
        if (Math.random() < randomProb) {
            // select zero node
            var funcIndex = Math.floor(Math.random() * this.NUM_ZERO_NODES);
        } else {
            var funcIndex = Math.floor(Math.random() * this.fnInfo.length);
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

    setImage: function(fn, width, height, imgId) {
        // FFV - worry about doing POST here?
        var img = document.getElementById(imgId);
        if (width > 0) {
            img.width = width;
        }
        if (height > 0) {
            img.height = height;
        }
        //alert(Object.toJSONString(fn));
        var newSrc=pngScript+"?w="+img.width+"&h="+img.height+"&f="+Object.toJSONString(fn);
        img.src = newSrc;
        //alert("done");
    },
    makeRandomImages: function() {
        for (var i = 1; i <= this.NUM_IMAGES; ++i) {
            var curId = 'img' + i;
            var fn = this.makeRandomFn(0.0);
            this.curFns[curId] = fn;
            this.setImage(fn, 140, 140, curId);
        }
    },
    init: function() {
        for (var i = 1; i <= this.NUM_IMAGES; ++i) {
            var curImg = document.getElementById('img' + i);
            curImg.onclick = function() {ppga.Class.selectImage(this.id);};
            curImg.className = 'img';
        }
        this.makeRandomImages();
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
        //alert(Object.toJSONString(fn));
        $('#fnDesc').empty();
        $('#fnDesc').append(Object.toJSONString(fn) + " " + Object.toJSONString(fn).length);
        this.setImage(fn, 140, 140, 'testImg');
    }
};

(function() {
    $(document).ready(function() {ppga.Class.init();});
})();
//]]>
