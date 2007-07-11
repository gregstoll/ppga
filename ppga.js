//<![CDATA[

function setImage(fn, width, height, imgId) {
    // FFV - worry about doing POST here?
    var img = document.getElementById(imgId);
    img.width = width;
    img.height = height;
    //alert(Object.toJSONString(fn));
    var newSrc=pngScript+"?width="+width+"&height="+height+"&func="+Object.toJSONString(fn);
    img.src = newSrc;
    //alert("done");
}

//$(document).ready(function() {setImage({'t':'x'}, 160, 120, 'testImg');});
//testFn = {'t':'atan','m':'c','ch':[{'t':'y'}]};
testFn = {'t':'add','m':'c','ch':[{'t':'x'},{'t':'y'}]};
$(document).ready(function() {setImage(testFn, 160, 120, 'testImg');});
/*function test() {
    setImage({'t':'atan','m':'c','ch':[{'t':'x'}]}, 160, 120, 'testImg');
}
window.setTimeout(test, 2000);*/
//]]>
