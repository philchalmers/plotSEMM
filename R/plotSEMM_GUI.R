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
                    
                    #Manual input
                    conditionalPanel(condition = "input.manual == true",
                                     sliderInput(inputId = "nclass",
                                                 label = "Number of latent classes:",
                                                 min = 1, max = 9, value = 2, step = 1),
                                     
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
                
#                 #preamble function here to grab input data
#                 GUI_setup <- function(input){
#                     
#                     return(rnorm(10))
#                 }
#                 
#                 
#                 output$main_plot <- renderPlot({
#                     ret <- GUI_setup(input)
#                     hist(ret)
#                 })                
                
            } #end server function
            
        ) #end list collector
        
        return(ret)
}
 