var editor;
var nytPopup;

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

    nytPopup = window.open("/viewer");

    $("body").bind("keydown", keyDown);
    $(editor).
    console.log("Ready!");
    console.log(nytPopup);
};

function run()
{
//    document.getElementById("editBox").style.right = '525px';
//    document.getElementById("runBox" ).style.display = 'block';
var s = editor.getValue();
    document.cookie = sourceCookie + "=" + encodeURIComponent(s)
        + "; max-age = " + (1000 * 365 * 24 * 60 * 60);
//    document.getElementById("form").submit();
//    editor.focus();

    var nytDiv = nytPopup.document.getElementById("runBox");
    $.post("/apiDocSpecs", { source : s }, function(data, status) {
        $(nytDiv).empty();
        $(nytDiv).append("<img src='/apiImage/"+data+"' />");
    });
}
function stop()
{
    document.getElementById("runBox" ).style.display = 'none';
    document.getElementById("editBox").style.right = '0px';
    window.display.location.href = "about:blank";
}

function keyDown(e) {
    if (e.keyCode == 115) {  // 115 - F4
        $("#runbutton").click();
    }
}

