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
        <script type="text/javascript" src="js/jquery-ui-1.8.20.custom.min.js"></script>
        <defaults/>
        <script type="text/javascript" src="js/editor.js?v=3"></script>
    </head>
    <body>
        <div id="editor_header">
            <h2 id="code_title">[title]</h2>
            <div id="github_logo">
                <a href="https://github.com/deggis/CV-web">
                    <img src='/images/github_octocat.png' />
                </a>
            </div>
            <div id="cv">
                <a href="http://aleator.github.com/CV/">
                    <h2>CV</h2>
                </a>
            </div>
            <div id="cv_docs">
                <a href="http://aleator.github.com/CV/dist/doc/html/CV/index.html">
                    <h2>CV</h2>
                    <h3>DOCS</h3>
                </a>
            </div>
        </div>
        <div id="editor_body" class="ui-corner-all">
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
            <div class="spacer">&nbsp;</div>
        </div>

        <div id="gallery">
          <div class="spacer">&nbsp;</div>
        </div>

    </body>
</html>
