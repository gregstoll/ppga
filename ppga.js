//<![CDATA[

var ppga;
if (!ppga) ppga = {};
ppga.Class = {
    // When making a random function, the probability that we automatically
    // select one with zero children increases by this much every time
    // (to make sure the trees don't go on forever)
    ZERO_CHILD_PROB : 0.03,
    NUM_ZERO_NODES : 3,
    fnInfo : [
       {'type': 'num', 'children': 0, 'aux': 'val'},
       {'type': 'x', 'children': 0},
       {'type': 'y', 'children': 0},
       {'type': 'atan', 'children': 1, 'needsMap' : true},
       {'type': 'add', 'children': 2, 'needsMap' : true}],

    makeRandomMapping: function(curInfo) {
        curInfo['m'] = (Math.random() < .5) ? 'c' : 'w';
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
                toReturn['ch'].push(this.makeRandomFn(randomProb+this.ZERO_CHILD_PROB));
            }
        }
        return toReturn;
    },

    setImage: function(fn, width, height, imgId) {
        // FFV - worry about doing POST here?
        var img = document.getElementById(imgId);
        img.width = width;
        img.height = height;
        //alert(Object.toJSONString(fn));
        var newSrc=pngScript+"?width="+width+"&height="+height+"&func="+Object.toJSONString(fn);
        img.src = newSrc;
        //alert("done");
    },

    randomFnTest: function() {
        var fn = this.makeRandomFn(0.0);
        //alert(Object.toJSONString(fn));
        $('#fnDesc').empty();
        $('#fnDesc').append(Object.toJSONString(fn) + " " + Object.toJSONString(fn).length);
        this.setImage(fn, 140, 140, 'testImg');
    }
};
//$(document).ready(function() {setImage({'t':'x'}, 160, 120, 'testImg');});
//testFn = {'t':'atan','m':'c','ch':[{'t':'y'}]};
(function() {
    testFn = {'t':'add','m':'c','ch':[{'t':'x'},{'t':'y'}]};
    $(document).ready(function() {ppga.Class.setImage(testFn, 140, 140, 'testImg');});
    //alert(Object.toJSONString(ppga.Class.makeRandomFn(0.0)));
})();
/*function test() {
    setImage({'t':'atan','m':'c','ch':[{'t':'x'}]}, 160, 120, 'testImg');
}
window.setTimeout(test, 2000);*/
//]]>
