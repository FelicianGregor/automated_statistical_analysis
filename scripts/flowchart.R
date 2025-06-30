#install.packages("DiagrammeR")
#install.packages("rsvg")
#install.packages("DiagrammeRsvg")
library(rsvg)
library(DiagrammeR)
library(DiagrammeRsvg)

#create a flowchart of the planned bachelor thesis project, depicting the main stages of the system

### hypothesis testing:####
flowchart = grViz("
digraph decision_tree {
  node [shape=box, style=filled, fillcolor=white]

 Start [shape=box, label=<
    <TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0'>
      <TR><TD><B>user input:</B></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; mode: hypothesis testing</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; define mathematical hypothesis</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; define distribution</FONT></TD></TR>
    </TABLE>
  >];

  Preprocess [shape=box, label=<
    <TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0'>
      <TR><TD><B>data preparation:</B></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; missing values</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; number of data points</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; </FONT><FONT COLOR='red' POINT-SIZE='10'>skewness - transformation</FONT></TD></TR>
    </TABLE>
  >]  
  
  // New Node
  A [label='fit GLM']
  B [label='model diagnostics']
  B [shape=box, label=<
    <TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0'>
      <TR><TD>model diagnostics</TD></TR>
      <TR><TD><FONT POINT-SIZE='10'>(using quantile residuals)</FONT></TD></TR>
    </TABLE>
  >]
  C [shape=box, label=<
    <TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0'>
      <TR><TD>reporting</TD></TR>
    </TABLE>
  >]
  
  D [label = 'model visualization']
  
  

  // Adjusted connections
  Start -> Preprocess  
  Preprocess -> A []
  A -> B [label='', tailport=s, headport=n]
  B -> D
  D -> C
}
")


# hussle for saving in high resolution:
library(htmlwidgets)
library(webshot)

# Save as HTML widget
saveWidget(flowchart, "./flowchart.html", selfcontained = TRUE)

# Convert HTML to PNG using webshot
webshot("./flowchart.html", file = "./flowchart.png", vwidth = 500, vheight = 1000, delay = 0.5, zoom  = 2)




###### version with prediction loop:####
flowchart = grViz("
digraph decision_tree {
  node [shape=box, style=filled, fillcolor=white]

 Start [shape=box, label=<
    <TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0'>
      <TR><TD><B>user input:</B></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; define mathematical hypothesis</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; define distribution</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; hypothesis testing / prediction</FONT><FONT COLOR='grey' POINT-SIZE='10'>/ data exploration</FONT></TD></TR>
    </TABLE>
  >];

  Preprocess [shape=box, label=<
    <TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0'>
      <TR><TD><B>data preparation:</B></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; missing values</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; correlation among predictor variables</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; number of data points</FONT></TD></TR>
      <TR><TD ALIGN='LEFT'><FONT POINT-SIZE='10'>&#8226; skewness </FONT></TD></TR>
    </TABLE>
  >]  
  
  // New Node
  A [label='fit GLM']
  B [label='model diagnostics']
  B [shape=box, label=<
    <TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0'>
      <TR><TD>model diagnostics</TD></TR>
      <TR><TD><FONT POINT-SIZE='10'>(using quantile residuals)</FONT></TD></TR>
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

###### version with test data box top right hand corner

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




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#####flowchart version for hypothesis testing####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##### Figure 1.2: general decision (first order) ####
grViz("
 digraph flowchart {
   // rankdir=LR; // left right layout
 
   node [shape=box, style=filled, fillcolor=white]
 
   Step1     [label='explanatory\nmodeling']
   Step2     [label='GLM']
   Step3     [label='frequentist\nstatistics']
   Step4     [label='hypothetico\ndeductive model']
   Step5     [label='procedure of\nhypothesis testing']
 }
")

##### Figure 1.2: flowchart for theorethical decisions (second order) #####
library(DiagrammeR)

grViz("
digraph flowchart {
  // rankdir=TB; // left right layout

  node [shape=box, style=filled, fillcolor=white]

  Step1 [label='Data Preparation']
  Step2 [label='Model Building']
  
  Step3 [label=<
    <table border='0' cellborder='0'>
      <tr><td align='center'>Model Diagnostics</td></tr>
      <tr><td align='left'><font point-size='8'>- influential data points (significant difference</font></td></tr>
      <tr><td align='left'><font point-size='8'>  expected &amp; detected outlier number)</font></td></tr>
    </table>
  >]
  
  Step4 [label='Visualization']
  Step5 [label='Reporting']

  Step1 -> Step2
  Step2 -> Step3
  Step3 -> Step4
  Step4 -> Step5
 }
 ")




##### Figure 1.2: flowchart implementation (third order)####
library(DiagrammeR)

grViz("
digraph flowchart {
  node [shape=box, style=filled, fillcolor=white]

  Step1 [label=<
    <font face='Courier'><b>prepare_data()</b></font>
  >]

  Step2 [label=<
    <font face='Courier'><b>build_model()</b></font>
  >]

  Step3 [label=<
    <table border='0' cellborder='0'>
      <tr><td align='center'><font face='Courier'><b>diagnose()</b></font></td></tr>
      <tr><td align='left'><font point-size='10'>- influential data points:</font></td></tr>
      <tr><td align='left'><font point-size='10'><font face='Courier'><b>DHARMa::testOutliers()</b></font></font></td></tr>
    </table>
  >]

  Step4 [label=<
    <font face='Courier'><b>plotting()</b></font>
  >]

  Step5 [label=<
    <font face='Courier'><b>report()</b></font>
  >]

  Step1 -> Step2
  Step2 -> Step3
  Step3 -> Step4
  Step4 -> Step5
}
")
##### Figure 2.1: flowchart for theoretical decisions AS: 2nd order #####
grViz("
digraph flowchart {
  rankdir=TB;

  node [shape=box, style=filled, fillcolor=white]

  Step1 [label=<
    <table border='0' cellborder='0'>
      <tr><td align='center'><b>Data Preparation</b></td></tr>
      <tr><td align='left'><font point-size='10'>- complete case analysis</font></td></tr>
    </table>
  >]

  Step2 [label=<
    <table border='0' cellborder='0'>
      <tr><td align='center'><b>Model Building &amp; Hypothesis Test</b></td></tr>
      <tr><td align='left'><font point-size='10'>- fit GLM</font></td></tr>
      <tr><td align='left'><font point-size='10'>- hypothesis test (2-sided Z-test) of GLM parameters</font></td></tr>
      <tr><td align='left'><font point-size='10'>- Bonferroni correction</font></td></tr>
      <tr><td align='left'><font point-size='10'>- Shapley values for global variable importance</font></td></tr>
    </table>
  >]

  Step3 [label=<
    <table border='0' cellborder='0'>
      <tr><td align='center'><b>Model Diagnostics</b></td></tr>
      <tr><td align='left'><font point-size='10'>- influential data points (significant difference of expected &amp; detected number of outliers) </font></td></tr>
      <tr><td align='left'><font point-size='10'>- dispersion (compare var of quantile residuals &amp; var of simulated observations)</font></td></tr>
      <tr><td align='left'><font point-size='10'>- residuals (quantile regression: significant deviations from theoretical quantiles)</font></td></tr>
      <tr><td align='left'><font point-size='10'>- collinearity (VIF &lt; 10, absolute correlation &lt; 0.7) </font></td></tr>
      <tr><td align='left'><font point-size='10'>- observations per predictor (&lt; 10) </font></td></tr>
    </table>
  >]

  Step4 [label=<
    <table border='0' cellborder='0'>
      <tr><td align='center'><b>Visualization</b></td></tr>
      <tr><td align='left'><font point-size='10'>box plot, scatter plot or contour plot with mean prediction &amp; 95% CI,</font></td></tr>
      <tr><td align='left'><font point-size='10'>plot type depends on number of categorical a&amp; continuous predictors</font></td></tr>
    </table>
  >, fillcolor=white]

  Step5 [label=<
    <table border='0' cellborder='0'>
      <tr><td align='center'><b>Reporting</b></td></tr>
      <tr><td align='left'><font point-size='10'>report structured by: input data,</font></td></tr>
      <tr><td align='left'><font point-size='10'>results, model diagnostics, miscellaneous</font></td></tr>
    </table>
  >]

  Step1 -> Step2
  Step2 -> Step3
  Step3 -> Step4
  Step4 -> Step5
  }
  ")

##### Figure 2.2: flowchart for implementation: 3rd order decisions ####
grViz("
digraph flowchart {
  rankdir=TB;

  node [shape=box, style=filled, fillcolor=white]

  // Node definitions
  Analyse [label=<
    <table border='0' cellborder='0'>
      <tr><td align='center'><b><font face='Courier'>analyse()</font></b></td></tr>
    </table>
  >]

  Step1 [label=<
    <table border='0' cellborder='0'>
      <tr><td align='center'><b><font face='Courier'>prepare_data()</font></b></td></tr>
      <tr><td align='left'><font point-size='10'>- complete case analysis: <font face='Courier'>na.omit()</font></font></td></tr>
    </table>
  >]

  Step2 [label=<
    <table border='0' cellborder='0'>
      <tr><td align='center'><b><font face='Courier'>build_model()</font></b></td></tr>
      <tr><td align='left'><font point-size='10'>- GLM with <font face='Courier'>VGAM::vglm()</font></font></td></tr>
      <tr><td align='left'><font point-size='10'>- Z-test of GLM estimates: <font face='Courier'>VGAM::wald.stat()</font> (within <font face='Courier'>vglm()</font>)</font></td></tr>
      <tr><td align='left'><font point-size='10'>- Bonferroni correction: manual, P value * num parameters GLM</font></td></tr>
      <tr><td align='left'><font point-size='10'>- Shapley values: <font face='Courier'>fastshap::explain()</font></font></td></tr>
    </table>
  >]

  Step3 [label=<
    <table border='0' cellborder='0'>
      <tr><td align='center'><b><font face='Courier'>diagnose()</font></b></td></tr>
      <tr><td align='left'><font point-size='10'>- influential observations: <font face='Courier'>DHARMa::testOutlier()</font></font></td></tr>
      <tr><td align='left'><font point-size='10'>- dispersion: <font face='Courier'>DHARMa::testDispersion()</font></font></td></tr>
      <tr><td align='left'><font point-size='10'>- residuals: <font face='Courier'>DHARMa::testQuantiles()</font></font></td></tr>
      <tr><td align='left'><font point-size='10'>- VIF: manual 1/(1-R2), correlation: <font face='Courier'>polycor::hetcor()</font></font></td></tr>
      <tr><td align='left'><font point-size='10'>- observations per predictor: manual</font></td></tr>
    </table>
  >]

  Step4 [label=<
    <table border='0' cellborder='0'>
      <tr><td align='center'><b><font face='Courier'>plotting()</font></b></td></tr>
      <tr><td align='left'><font point-size='10'><font face='Courier'>boxplot()</font>, <font face='Courier'>plot()</font> or <font face='Courier'>contour()</font> with <font face='Courier'>image()</font> for heatmap</font></td></tr>
    </table>
  >]

  Step5 [label=<
    <table border='0' cellborder='0'>
      <tr><td align='center'><b><font face='Courier'>report()</font></b></td></tr>
      <tr><td align='left'><font point-size='10'>dynamically write quarto document, render as PDF or HTML file</font></td></tr>
    </table>
  >]

  // Flow connections
  Analyse -> Step1
  Step1 -> Step2
  Step2 -> Step3
  Step3 -> Step4
  Step4 -> Step5

  // Force Analyse and Step1 to be on the same horizontal level
  { rank = same; Analyse; Step1; }
}
")
  














