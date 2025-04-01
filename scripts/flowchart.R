#install.packages("DiagrammeR")
install.packages("rsvg")
install.packages("DiagrammeRsvg")
library(rsvg)
library(DiagrammeR)
library(DiagrammeRsvg)

### without test data on data box
flowchart = grViz("
digraph decision_tree {
  node [shape=box, style=filled, fillcolor=white]

 Start [shape=box, label=<
    <TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0'>
      <TR><TD><B>user input:</B></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; define x and y variable in data set</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; define distribution family</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; hypothesis testing / prediction</FONT><FONT COLOR='grey' POINT-SIZE='10'>/ data exploration</FONT></TD></TR>
    </TABLE>
  >];

  Preprocess [shape=box, label=<
    <TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0'>
      <TR><TD><B>data checking:</B></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; missing values</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; correlation among predictor variables</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; number of data points</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; skewness </FONT></TD></TR>
    </TABLE>
  >]  
  
  // New Node
  A [label='define & fit (V)GLM']
  B [label='model diagnostics (DHARMa) OK?']
  B [shape=box, label=<
    <TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0'>
      <TR><TD>model diagnostics OK?</TD></TR>
      <TR><TD><FONT POINT-SIZE='10'>(using the DHARMa-package)</FONT></TD></TR>
    </TABLE>
  >]
  C [label='adjust model']
  E [shape=box, label=<
    <TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0'>
      <TR><TD>generate .txt output file from list</TD></TR>
      <TR><TD><FONT COLOR='grey' POINT-SIZE='10'>optional: sophisticated PDF/html report</FONT></TD></TR>
    </TABLE>
  >]
  
  

  // Adjusted connections
  Start -> Preprocess  [arrowhead = none]
  Preprocess -> A []
  A -> B [label='', tailport=s, headport=n]
  B -> C [label='No']
  B -> E [label='Yes']

  C -> A [labeldistance=2, labelangle=180, tailport=w, headport=w, label=<
    <TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0'>
      <TR><TD><B>adjust:</B></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; distribution</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; functional relation</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; handling outliers</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; model selection</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; interactions</FONT></TD></TR>
    </TABLE>
  >]
}
")


grViz("
digraph decision_tree {
  node [shape=box, style=filled, fillcolor=white]

  Start [shape=box, label=<
    <TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0'>
      <TR><TD><B>user input:</B></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; define x and y variable in data set</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; define distribution family</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; hypothesis testing or prediction</FONT></TD></TR>
    </TABLE>
  >];
  Preprocess [label='extensive data checking', color = grey, fontcolor = grey]  // New Node
  A [label='define & fit (V)GLM']
  B [label='model diagnostics OK?']
  C [label='adjust model']
  E [label='output']
  F [label='sophisticated PDF/html report', color=grey, fontcolor=grey]
  G [label='RF for non-linear interaction', color=grey, fontcolor=grey]
  H [shape=none, label=<
    <TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0'>
      <TR><TD><B>test system on data with:</B></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; linear/non-linear relationships</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; one/several predictor variables</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; spatial/temporal autocorrelation</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; missing values</FONT></TD></TR>
    </TABLE>
  >]

  

  // Adjusted connections
  Start -> Preprocess  [arrowhead = none]
  Preprocess -> A []
  A -> B [label='', tailport=s, headport=n]
  B -> C [label='No']
  B -> E [label='Yes']

  C -> A [labeldistance=2, labelangle=180, tailport=w, headport=w, label=<
    <TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0'>
      <TR><TD><B>adjust</B></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; distribution family</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; functional relation</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; handling outliers</FONT></TD></TR>
    </TABLE>
  >]

  {rank=same; E; F;}
  E -> F [color=grey]

  {rank=same; B; G;}
  B -> G [color=grey]
  G -> F [color=grey]
  {rank=same; Preprocess; H;}
}
")

