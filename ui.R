library(shiny)
library(markdown)

load("./plans.rda")

shinyUI(pageWithSidebar(
  
  headerPanel("Benefit Plan Comparison"),
  
  sidebarPanel(
    
    selectInput(inputId = "class", label = "Choose plan type:",
                list("Employee only" = "emp", "Employee and spouse" = "emp_spouse",
                     "Employee and child" = "emp_child", "Employee and family" = "emp_fam")),
    
    HTML("<br><br><i>Note:</i> HSA/HCRA contribution limits are $3,300 for employee only,",
         "and $6,550 for families. This is the <b>total</b>, which includes the $625/$1,250",
         "company contribution received by plans B and C<br><br>"),
    sliderInput(inputId = "hsa", label = "Enter your elective HSA/HCRA contribution:",
                min = 0, max = 5000, value = 0, step = 100),
    
    conditionalPanel(
      condition = "input.class != 'emp'",
      sliderInput(inputId = "members", label = "Number of individuals on plan:",
                  min = 1, max = 15, value = 1, step = 1)
    ),
    
    sliderInput(inputId = "max_pred", label = "Adjust range for sliders below:",
                min = 5000, max = 100000, value = 5000, step = 5000),
    
    HTML("<br><i>Enter low/high expense estimates for each member on plan:<br><br></i>"),
    
    uiOutput("sliders"),
    
    HTML("<hr>Written by John Henderson",
         "<br>jw [dot] hendy [at] gmail [dot] com")
    
  ),
  
  mainPanel(
    tabsetPanel(  
      tabPanel("Cost comparison", 
               plotOutput(outputId = "main_plot", width = "100%"),
               HTML("<i>Table of values for best case (lowest predictions)</i>"),
               tableOutput(outputId = "bar_summ_best"),
               HTML("<i>Table of values for worst case (highest predictions)</i>"),
               tableOutput(outputId = "bar_summ_worst")),
      tabPanel("Monthly flow (ave.)", plotOutput(outputId = "flow_ave")),
      tabPanel("Monthly flow (all in Jan.)", plotOutput(outputId = "flow_jan")),
      tabPanel("Guide", includeMarkdown("./guide.Rmd"), tags$head(
        tags$script(src="https://c328740.ssl.cf1.rackcdn.com/mathjax/2.0-latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
      ))
    )
  )))
