var editor;
var nytPopup;

var demos = [
    { source: 'pixelwise.hs'},
    { source: 'empty.hs' },
    { source: 'min.hs' },
    { source: 'get.hs' },
    { source: 'lena_misc.hs' }
];

window.onload = function() {
    editor = CodeMirror.fromTextArea(
        document.getElementById("editor"), {
            matchBrackets: true,
            indentWithTabs: false,
            indentUnit: 4,
            tabMode: "shift",
            lineNumbers: true,
            theme: "glossweb",
            onCursorActivity: function() {
                editor.setLineClass(hlLine, null);
                hlLine = editor.setLineClass(editor.getCursor().line, "activeline");
                }
            }

        );

    var hlLine = editor.setLineClass(0, "activeline");

    var all = document.cookie;
    var list = all.split("; ");
    var found = false;
    for (var i = 0; i < list.length; i++)
    {
        var cookie = list[i];
        var p = cookie.indexOf("=");
        var name = cookie.substring(0,p);
        if (name == sourceCookie)
        {
            var value = decodeURIComponent(cookie.substring(p+1));
            editor.setValue(value);
            found = true;
            break;
        }
    }

    if (!found)
    {
        editor.setValue(initialSource);
    }

    //stop();
    editor.focus();


    if(popup) {
//        console.log("Opening popup viewer.");
        nytPopup = window.open("/viewerPopup");
//        console.log(nytPopup);
    }
    else {
        console.log("Constructing div viewer.");
        $("#image_container").append("<div id='runBox'></div>");
    }

    $("body").bind("keydown", keyDown);
    $("#runlink").button();
//    console.log("Ready!");

    addDemosToGallery();

};

function addDemosToGallery() {
    $(demos).each(function(i) {
        var demo = demos[i];
        var sourceFn = demo["source"];
        var preventCache = "?"+(new Date().getTime());
        // FATAL: server side breaks with simultaneous attempts.
        setTimeout(function() {
            $.get("/demos/"+sourceFn+preventCache, function(source) {
                $.post("/eval", { source : source }, function(data, status) {
                    var response = parseResponse(data);
                    if (response["status"] == "ok") {
                        $("#gallery").append("<div class='gallery_image'><a href='javascript:activateDemo(\""+sourceFn+"\")'><img src='/thumbnail/"+response["hash"]+"' width='70' height='70' alt='' /></a></div>");
                    }
                    else {
//                        console.log("Rendering a demo has failed! Reason: "+response["reason"]);
                    }
                });
            });
        }, i*1500);
    });
}

function activateDemo(sourceFn) {
    $.get("/demos/"+sourceFn+"?"+(new Date().getTime()), function(source) {
        editor.setValue(source);

        $("#code_title").fadeOut(300, function() {
            $(this).html(sourceFn).fadeIn();
        });
        $("#editor_body").fadeOut(300, function() {
            run();
            $("#editor_body").fadeIn();
        });

    });
}

function setDemo(i) {
    demo = demos[i];
    activateDemo(demo["source"]);
}

function run()
{
    var s = editor.getValue();
    document.cookie = sourceCookie + "=" + encodeURIComponent(s)
        + "; max-age = " + (1000 * 365 * 24 * 60 * 60);
    var nytDiv;
    if(popup) {
        nytDiv = nytPopup.document.getElementById("runBox");
    }
    else {
        nytDiv = $("#runBox");
//        console.log(nytDiv);
    }

    $.post("/eval", { source : s }, function(data, status) {
        var response = parseResponse(data);
        if (response["status"] == "ok") {
            $(nytDiv).empty();
            $(nytDiv).append("<img src='/image/"+response["hash"]+"' />");
        }
        else {
            $(nytDiv).empty();
            $("#editor_body").effect('shake', {}, 80);
            $(nytDiv).append("<pre class='code_errors'>"+response["reason"]+"</pre>");
        }
    });
}

function parseResponse(data) {
    return eval("["+data+"]")[0]; // FIXME: why doesn't work without array
}

function keyDown(e) {
    if (e.keyCode == 115) {  // 115 - F4
        $("#runbutton").click();
    }
}

