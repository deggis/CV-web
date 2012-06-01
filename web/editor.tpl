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
        <script type="text/javascript" src="js/editor.js?v=3"></script>
    </head>
    <body>
        <div id="editor_header">
            <h2 id="code_title">[title]</h2>
        </div>
        <div id="editor_body" class="ui-corner-all">
<!--            <div style="height: 70px">
                <div>
                    <intro/>
                </div>
            </div> -->
            <div id="editor_image">
                <div id="image_container">
                </div>
            </div>
            <div id="editor_code" class="ui-corner-right">
                <textarea id="editor" name="source">Loading...</textarea>
                <div id="editor_buttons">
                    <input id="runbutton" type="button" value="Process"  onclick="run()"/>
                </div>
            </div>
            <div class="spacer">
              &nbsp;
            </div>
        </div>
    </body>
</html>
