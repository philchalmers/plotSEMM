#' PlotSEMM GUI
#' 
#' Graphical user interface with the shiny package. Supports manual input as well as importing
#' from precomputed Mplus files. 
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
    numberInputRow<-function (inputId, label, value = "") 
    {        
        div(style="display:inline-block",
            tags$label(label, `for` = inputId), 
            tags$input(id = inputId, type = "number", value = value,class="input-small"))
    }
    
    #custom file.choose() function (only works locally)
    file.choose2 <- function(...) {
        pathname <- NULL
        tryCatch({
            pathname <- file.choose()
            }, error = function(ex) {})
        pathname
    }
        
        ret <- list(
            
            #--------------------------------------------------------------------
            ui = pageWithSidebar(
                
                # Application title
                headerPanel("Plot_SEMM"),
                
                sidebarPanel(
                    
                    h5('Please specify your data either by importing an Mplus output 
                       file or manually'),
                    
                    selectInput(inputId="method", label="Select how you would like to input the parameters:",
                                choices=c("Mplusfile"="Mplusfile", "Manually"="Manually", " "=" "), selected=" "),
                    
                    #Mplus input
                    conditionalPanel(condition = "input.method == 'Mplusfile'",
                                     textInput(inputId='Mpath', label='Directory containing Mplus files:',
                                               value=getwd())
                    ),
                    
                    selectInput(inputId="plottype",label="Type of plot to generate:",
                                choices=c("contour"="contour", 'probability'='probability'), selected="contour"),
                    
                    #Manual input
                    conditionalPanel(condition = "input.method == 'Manually'",
                                     sliderInput(inputId = "nclass",
                                                 label = "Number of latent classes:",
                                                 min = 2, max = 7, value = 2, step = 1),
                                     
                                     hr(),    
                                     h6('Class 1:'),
                                     numberInputRow(inputId="pi1", label="pi"),
                                     numberInputRow(inputId="alpha1.1", label="alpha 1"),
                                     numberInputRow(inputId="alpha2.1", label="alpha 2"),
                                     numberInputRow(inputId="beta21.1", label="beta 21"),
                                     numberInputRow(inputId="psi11.1", label="psi 11"),
                                     numberInputRow(inputId="psi22.1", label="psi 22"),
                                     
                                     conditionalPanel(condition = "input.nclass > 1",
                                                      hr(),
                                                      h6('Class 2:'),
                                                      numberInputRow(inputId="pi2", label="pi"),
                                                      numberInputRow(inputId="alpha1.2", label="alpha 1"),
                                                      numberInputRow(inputId="alpha2.2", label="alpha 2"),
                                                      numberInputRow(inputId="beta21.2", label="beta 21"),
                                                      numberInputRow(inputId="psi11.2", label="psi 11"),
                                                      numberInputRow(inputId="psi22.2", label="psi 22")
                                                      ),
                                     
                                     conditionalPanel(condition = "input.nclass > 2",
                                                      hr(),
                                                      h6('Class 3:'),
                                                      numberInputRow(inputId="pi3", label="pi"),
                                                      numberInputRow(inputId="alpha1.3", label="alpha 1"),
                                                      numberInputRow(inputId="alpha2.3", label="alpha 2"),
                                                      numberInputRow(inputId="beta21.3", label="beta 21"),
                                                      numberInputRow(inputId="psi11.3", label="psi 11"),
                                                      numberInputRow(inputId="psi22.3", label="psi 22")
                                     ),
                                     
                                     conditionalPanel(condition = "input.nclass > 3",
                                                      hr(),
                                                      h6('Class 4:'),
                                                      numberInputRow(inputId="pi4", label="pi"),
                                                      numberInputRow(inputId="alpha1.4", label="alpha 1"),
                                                      numberInputRow(inputId="alpha2.4", label="alpha 2"),
                                                      numberInputRow(inputId="beta21.4", label="beta 21"),
                                                      numberInputRow(inputId="psi11.4", label="psi 11"),
                                                      numberInputRow(inputId="psi22.4", label="psi 22")
                                     ),
                                     
                                     conditionalPanel(condition = "input.nclass > 4",
                                                      hr(),
                                                      h6('Class 5:'),
                                                      numberInputRow(inputId="pi5", label="pi"),
                                                      numberInputRow(inputId="alpha1.5", label="alpha 1"),
                                                      numberInputRow(inputId="alpha2.5", label="alpha 2"),
                                                      numberInputRow(inputId="beta21.5", label="beta 21"),
                                                      numberInputRow(inputId="psi11.5", label="psi 11"),
                                                      numberInputRow(inputId="psi22.5", label="psi 22")
                                     ),
                                     
                                     conditionalPanel(condition = "input.nclass > 5",
                                                      hr(),
                                                      h6('Class 6:'),
                                                      numberInputRow(inputId="pi6", label="pi"),
                                                      numberInputRow(inputId="alpha1.6", label="alpha 1"),
                                                      numberInputRow(inputId="alpha2.6", label="alpha 2"),
                                                      numberInputRow(inputId="beta21.6", label="beta 21"),
                                                      numberInputRow(inputId="psi11.6", label="psi 11"),
                                                      numberInputRow(inputId="psi22.6", label="psi 22")
                                     ),
                                     
                                     conditionalPanel(condition = "input.nclass > 6",
                                                      hr(),
                                                      h6('Class 7:'),
                                                      numberInputRow(inputId="pi7", label="pi"),
                                                      numberInputRow(inputId="alpha1.7", label="alpha 1"),
                                                      numberInputRow(inputId="alpha2.7", label="alpha 2"),
                                                      numberInputRow(inputId="beta21.7", label="beta 21"),
                                                      numberInputRow(inputId="psi11.7", label="psi 11"),
                                                      numberInputRow(inputId="psi22.7", label="psi 22")
                                     )
                                     
                    ),

                    hr(),

                    submitButton(text = "Submit")
                    
                    ), #end sidebarPanel
                
                #------------------------------------------------------------------------
                
                mainPanel(
                    plotOutput(outputId = "main_plot", height = '800px', width = "100%")
                    )
            
                ), #end pageWithSidebar
            
            #--------------------------------------------------------------------
            
            server = function(input, output) {
                
                #preamble function here to grab input data
                GUI_setup <- function(input){
                    
                    nclass <- input$nclass
                    ret <- NULL
                    if(input$method == 'Manually'){
                        
                        pi <- alpha1 <- alpha2 <- beta21 <- psi11 <- psi22 <- numeric(nclass)
                        if(is.na(input$pi2))
                            return(NULL)
                        Names <- names(input)
                        pi.names <- paste0('pi', 1:7)
                        alpha1.names <- paste0('alpha1.', 1:7)
                        alpha2.names <- paste0('alpha2.', 1:7)
                        beta21.names <- paste0('beta21.', 1:7)
                        psi11.names <- paste0('psi11.', 1:7)
                        psi22.names <- paste0('psi22.', 1:7)                        
                        for(i in 1L:nclass){
                            pi[i] <- input[[Names[which(Names %in% pi.names)[i]]]]
                            alpha1[i] <- input[[Names[which(Names %in% alpha1.names)[i]]]]
                            alpha2[i] <- input[[Names[which(Names %in% alpha2.names)[i]]]]
                            beta21[i] <- input[[Names[which(Names %in% beta21.names)[i]]]]
                            psi11[i] <- input[[Names[which(Names %in% psi11.names)[i]]]]
                            psi22[i] <- input[[Names[which(Names %in% psi22.names)[i]]]]                            
                        }
                        test <- c(pi, alpha1, alpha2, beta21, psi11, psi22)
                        if(any(is.na(test)))
                            stop('Must include all input values for each class')
                        ret <- plotSEMM_setup(pi, alpha1, alpha2, beta21, psi11, psi22)
                    } else if(input$method == 'Mplusfile'){
                        
                        if(!is.null(input$Mpath)){
                            files <- dir()
                            file <- files[grepl("*\\.out$", files)]
                            if(!length(file))
                                return(NULL)
                            if(length(file) > 1L)
                                stop('Multiple .out files in specifed directory')
                            read <- MplusAutomation::readModels(file)
                        } else {
                            return(NULL)
                        }
                        ovars <- strsplit(toupper(read$input$variable$names), split = ' ')[[1L]]
                        pi <- read$class_counts$modelEstimated$proportion
                        pars <- read$parameters[[1L]]
                        pars <- pars[!(pars$param %in% ovars), ] #latents only
                        tmp <- min(which(grepl("*\\.ON$", pars$paramHeader)))
                        ON <- pars$paramHeader[tmp]
                        DV <- strsplit(ON, '.ON')[[1L]]
                        IV <- pars$param[tmp]
                        pars <- pars[pars$param %in% c(IV,DV), ] 
                        alpha1 <- pars$est[pars$paramHeader == 'Means']
                        alpha2 <- pars$est[pars$paramHeader == 'Intercepts']
                        beta21 <- pars$est[pars$paramHeader == ON]
                        psi11 <- pars$est[pars$paramHeader == 'Variances']
                        psi22 <- pars$est[pars$paramHeader == 'Residual.Variances']
                        ret <- plotSEMM_setup(pi, alpha1, alpha2, beta21, psi11, psi22)
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
 