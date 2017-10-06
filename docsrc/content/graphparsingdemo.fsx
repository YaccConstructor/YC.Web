(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
YC.GraphParsingDemo
======================

Documentation
YC.GraphParsingDemo is web application for parsing and visualizing graphs and SPPFs (Shared Packed Parse Forests). <br>
This app can also visuailze formal subgraphs, filter out redundant nodes from SPPF and search for minimal length path betweeen two specified vertices of the input graph.

Tutorial
========================

Start
---------------

To start YCGraphParsingDemo application you need to type in grammar and graph and press "SHOW GRAPH" button. 
<img src="img/filled.jpg" alt="filledUI"/>
You can also upload files from your device. <br>
If you want to show formal subgraph of input graph and/or remove redundant nodes from SPPF, use checkboxes under the graph input form.<br>
<img src="img/check.jpg" alt="checkboxes"/> 
<br>
When you press "SHOW GRAPH" button, two new buttons will appear:<br>
<img src="img/show.jpg" alt="showtime"/>
<br>To see graph or SPPF visualizations press their respective buttons. 
<br>To extract minimal length path between two specific vertices of the input graph write their numbers in special form and press "FIND PATH".
<img src="img/path.jpg" alt="path"/><br>
After that you will see two other visualization controls as you did in previous step. 
Note that unchecking one of the checkboxes, clicking on "SHOW GRAPH" the second time or changing text in the input forms will reset visualization. 
Clicking on one of visualisation windows will refresh it and allow you to change the graph layout.


Using examples
------------------
 * Example 1 <img src="img/1.jpg" alt="example1"/>

 * Example 2 <img src="img/2.jpg" alt="example2"/>

 * Example 2 (part 2) <img src="img/3.jpg" alt="example3"/>

 * Example 3 <img src="img/4.jpg" alt="example4"/>



*)