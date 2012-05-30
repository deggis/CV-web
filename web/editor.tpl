<!DOCTYPE html>
<html>
    <head>
        <link rel="stylesheet" href="codemirror.css">
        <link rel="stylesheet" href="theme/glossweb.css">
        <style type="text/css">
            html { height: 100% }
            body { position: absolute;
                   left: 2px;
                   right: 2px;
                   top: 2px;
                   bottom: 2px;
                   margin: 0 0 0 0 }
            .CodeMirror {
                border: dotted gray 1px;
                height: 100%;
                overflow: auto;
                max-height: 100%;
                }
            .CodeMirror-scroll {
                height: 100%;
                max-height: 100%;
                }
        </style>
        <script type="text/javascript" src="codemirror-compressed.js"></script>
        <script type="text/javascript" src="jquery-1.7.2.min.js"></script>
        <defaults/>
        <script type="text/javascript" src="editor.js"></script>
        <script type="text/javascript">

        </script>
    </head>
    <body>
        <div style="height: 70px">
            <div>
                <intro/>
            </div>
            <hr style="clear:both">
        </div>
        <div style="position: absolute; top: 70px; bottom: 0px; left: 0px; right: 0px">
            <div id="editBox" style="position: absolute; top: 0px; bottom: 0px; left: 0px; right: 525px">
                <textarea style="width: 100%; height: 100%" id="editor" name="source">Loading...</textarea>
            </div>
        </div>
        <div style="position: fixed; bottom: 20px; right: 20px">
            <input id="runbutton" type="button" value="Run"  onclick="run()"/>
            <input type="button" value="Stop" onclick="stop()"/>
        </div>
    </body>
</html>

