#' PlotSEMM GUI
#' 
#' Something
#' 
#' @aliases plotSEMM_GUI
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @keywords shiny GUI
#' @export plotSEMM_GUI
#' @examples 
#' \dontrun{ 
#' plotSEMM_GUI()
#' }
plotSEMM_GUI <- function(){
    require(shiny)
    runApp(plotSEMM_GUI.internal())
}

plotSEMM_GUI.internal <- function(){
    
    #custom function to put user text input boxes side-by-side
    textInputRow<-function (inputId, label, value = "") 
    {
        div(style="display:inline-block",
            tags$label(label, `for` = inputId), 
            tags$input(id = inputId, type = "numeric", value = value,class="input-small"))
    }
        
        ret <- list(
            
            #--------------------------------------------------------------------
            ui = pageWithSidebar(
                
                # Application title
                headerPanel("Plot_SEMM"),
                
                sidebarPanel(
                    
                    h5('Please specify your data either by importing an Mplus output 
                       file or manually'),
                    
                    checkboxInput(inputId = "manual",
                                  label = "Specify input manually?",
                                  value = FALSE),
                    
                    #Mplus input
                    conditionalPanel(condition = "input.manual == false",
                                     fileInput('Mplusfile', label='Locate Mplus output file (.out)',
                                               accept=c('text/plain', '.out'))
                    ),
                    
                    selectInput(inputId="plottype",label="Type of plot to generate:",
                                choices=c("contour"="contour", 'probability'='probability'), selected="contour"),
                    
                    #Manual input
                    conditionalPanel(condition = "input.manual == true",
                                     sliderInput(inputId = "nclass",
                                                 label = "Number of latent classes:",
                                                 min = 1, max = 7, value = 2, step = 1),
                                     
                                     hr(),
                                     submitButton(text = "Update # of classes"),
                                     
                                     hr(),    
                                     h6('Class 1:'),
                                     textInputRow(inputId="pi1", label="pi"),
                                     textInputRow(inputId="alpha1.1", label="alpha 1"),
                                     textInputRow(inputId="alpha1.2", label="alpha 2"),
                                     textInputRow(inputId="beta1.21", label="beta 21"),
                                     textInputRow(inputId="phi1.11", label="phi 11"),
                                     textInputRow(inputId="phi1.22", label="phi 22"),
                                     
                                     conditionalPanel(condition = "input.nclass > 1",
                                                      hr(),
                                                      h6('Class 2:'),
                                                      textInputRow(inputId="pi2", label="pi"),
                                                      textInputRow(inputId="alpha2.1", label="alpha 1"),
                                                      textInputRow(inputId="alpha2.2", label="alpha 2"),
                                                      textInputRow(inputId="beta2.21", label="beta 21"),
                                                      textInputRow(inputId="phi2.11", label="phi 11"),
                                                      textInputRow(inputId="phi2.22", label="phi 22")
                                                      ),
                                     
                                     conditionalPanel(condition = "input.nclass > 2",
                                                      hr(),
                                                      h6('Class 3:'),
                                                      textInputRow(inputId="pi3", label="pi"),
                                                      textInputRow(inputId="alpha3.1", label="alpha 1"),
                                                      textInputRow(inputId="alpha3.2", label="alpha 2"),
                                                      textInputRow(inputId="beta3.21", label="beta 21"),
                                                      textInputRow(inputId="phi3.11", label="phi 11"),
                                                      textInputRow(inputId="phi3.22", label="phi 22")
                                     ),
                                     
                                     conditionalPanel(condition = "input.nclass > 3",
                                                      hr(),
                                                      h6('Class 4:'),
                                                      textInputRow(inputId="pi4", label="pi"),
                                                      textInputRow(inputId="alpha4.1", label="alpha 1"),
                                                      textInputRow(inputId="alpha4.2", label="alpha 2"),
                                                      textInputRow(inputId="beta4.21", label="beta 21"),
                                                      textInputRow(inputId="phi4.11", label="phi 11"),
                                                      textInputRow(inputId="phi4.22", label="phi 22")
                                     ),
                                     
                                     conditionalPanel(condition = "input.nclass > 4",
                                                      hr(),
                                                      h6('Class 5:'),
                                                      textInputRow(inputId="pi5", label="pi"),
                                                      textInputRow(inputId="alpha5.1", label="alpha 1"),
                                                      textInputRow(inputId="alpha5.2", label="alpha 2"),
                                                      textInputRow(inputId="beta5.21", label="beta 21"),
                                                      textInputRow(inputId="phi5.11", label="phi 11"),
                                                      textInputRow(inputId="phi5.22", label="phi 22")
                                     ),
                                     
                                     conditionalPanel(condition = "input.nclass > 5",
                                                      hr(),
                                                      h6('Class 6:'),
                                                      textInputRow(inputId="pi6", label="pi"),
                                                      textInputRow(inputId="alpha6.1", label="alpha 1"),
                                                      textInputRow(inputId="alpha6.2", label="alpha 2"),
                                                      textInputRow(inputId="beta6.21", label="beta 21"),
                                                      textInputRow(inputId="phi6.11", label="phi 11"),
                                                      textInputRow(inputId="phi6.22", label="phi 22")
                                     ),
                                     
                                     conditionalPanel(condition = "input.nclass > 6",
                                                      hr(),
                                                      h6('Class 7:'),
                                                      textInputRow(inputId="pi7", label="pi"),
                                                      textInputRow(inputId="alpha7.1", label="alpha 1"),
                                                      textInputRow(inputId="alpha7.2", label="alpha 2"),
                                                      textInputRow(inputId="beta7.21", label="beta 21"),
                                                      textInputRow(inputId="phi7.11", label="phi 11"),
                                                      textInputRow(inputId="phi7.22", label="phi 22")
                                     )
                                     
                    ),

                    hr(),

                    submitButton(text = "Submit")
                    
                    ), #end sidebarPanel
                
                #------------------------------------------------------------------------
                
                mainPanel(
                    plotOutput(outputId = "main_plot", height = "700px", width = "700px")
                    )
            
                ), #end pageWithSidebar
            
            #--------------------------------------------------------------------
            
            server = function(input, output) {
                
                #preamble function here to grab input data
                GUI_setup <- function(input){
                    
                    nclass <- input$nclass
                    
                    if(input$manual){
                        pi <- alpha1 <- alpha2 <- beta21 <- psi11 <- psi22 <- numeric(nclass)                        
#                         if(is.null(input$pi1))
#                             return(NULL)
                        
                        #TEMP
                        pi <- c(0.602, 0.398)                        
                        alpha1 <- c(3.529, 2.317)                        
                        alpha2 <- c(0.02, 0.336)                        
                        beta21 <- c(0.152, 0.053)                        
                        psi11 <- c(0.265, 0.265)                        
                        psi22 <- c(0.023, 0.023)                        
                        test <- c(pi, alpha1, alpha2, beta21, psi11, psi22)
                        
                        if(any(is.na(test)))
                            stop('Must include all input values')
                        ret <- plotSEMM_setup(pi, alpha1, alpha2, beta21, psi11, psi22)
                    } else {
                        
                        if(!is.null(input$Mplusfile)){
                            ret <- read(input$Mplusfile)
                        } else {
                            return(NULL)
                        }
                        
                        
                    }
                    return(ret)
                }
                
                #-----------------------------------------------------------------

                output$main_plot <- renderPlot({
                    ret <- GUI_setup(input)
                    if(!is.null(ret)){
                        plottype <- input$plottype
                        if(plottype == 'contour') plotSEMM_contour(ret)
                        if(plottype == 'probability') plotSEMM_probability(ret)
                    }
                })                
                
            } #end server function
            
        ) #end list collector
        
        return(ret)
}
 