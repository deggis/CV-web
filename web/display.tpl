<!DOCTYPE html>
<html>
    <head>
        <title>Drawing</title>
        <link rel="stylesheet" type="text/css" href="tooltip.css">
        <style type="text/css">
        body {
            margin: 0 0 0 0;
            text-align: center;
            }
        </style>
        <!--[if IE]>
        <script type="text/javascript" src="excanvas.js"></script>
        <![endif]-->
        <script type="text/javascript" src="EventSource.js"></script>
        <displayScript/>
        <script type="text/javascript" src="draw.js"></script>
    </head>
    <body onload="init()" style="overflow:hidden">
        <canvas id="screen"
                width="500"
                height="500"
                tabindex="0"
                style="border:solid black 1px">
        </canvas>
    </body>
</html>

