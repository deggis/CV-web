<!DOCTYPE html>
<html>
    <head>
        <link rel="stylesheet" href="css/codemirror.css">
        <link rel="stylesheet" href="css/theme/glossweb.css">
        <link type="text/css" href="css/humanity/jquery-ui-1.8.20.custom.css" rel="stylesheet" />
        <link type="text/css" href="css/cvweb.css?v=2" rel="stylesheet" />
        <link type="text/css" href="css/editor.css?v=2" rel="stylesheet" />
        <settings />
        <script type="text/javascript" src="js/codemirror-compressed.js"></script>
        <script type="text/javascript" src="js/jquery-1.7.2.min.js"></script>
        <defaults/>
        <script type="text/javascript" src="js/editor.js?v=2"></script>
    </head>
    <body>
        <div style="height: 70px">
            <div>
                <intro/>
            </div>
            <hr style="clear:both">
        </div>
        <div id="editor_left">
            <textarea id="editor" name="source">Loading...</textarea>
            <div id="editor_buttons">
                <input id="runbutton" type="button" value="Process"  onclick="run()"/>
            </div>
        </div>
        <div id="editor_right">
            <div id="imageContainer">
            </div>
        </div>
    </body>
</html>
