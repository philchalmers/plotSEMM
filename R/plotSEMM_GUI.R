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
        
        ret <- list(
            
            #--------------------------------------------------------------------
            ui = pageWithSidebar(
                
                # Application title
                headerPanel("PlotSEMM"),
                
                sidebarPanel(
                    
                    h4('Please specify your data either by importing an Mplus output 
                       file or manually.'),
                    h5('Note: The \'Mplus file\' option requires that only one
                       Mplus output file (.out) is in the refered directory.'),
                    
                    selectInput(inputId="method", label="Select how you would like to input the parameters:",
                                choices=c("Mplus File"="Mplusfile", "Manual Input"="Manually", " "=" "), selected=" "),
                    
                    #Mplus input
                    conditionalPanel(condition = "input.method == 'Mplusfile'",
                                     textInput(inputId='Mpath', label='Directory containing Mplus file:',
                                               value=getwd())
                    ),
                    
                    selectInput(inputId="plottype",label="Type of plot to generate:",
                                choices=c("contour"="contour", 'probability'='probability',
                                          "confidence interval (Mplus input only)"="ci"), 
                                selected="contour"),
                    
                    conditionalPanel(condition = "input.plottype == 'ci'",
                                     selectInput(inputId='CI', label='Confidence interval',
                                                 choices=c("95%", "90%"), selected="95%")
                    ),
                    
                    conditionalPanel(condition = "input.plottype == 'ci'",
                                     shiny::checkboxInput(inputId='plot_deltaci', 
                                                          label='Plot the confidence intervals using
                                                          the Delta method?',
                                                          value=TRUE)
                    ),
                    
                    conditionalPanel(condition = "input.plottype == 'ci'",
                                     shiny::checkboxInput(inputId='plot_bsci', 
                                                          label='Plot the confidence intervals using
                                                          the Bootstrap method?',
                                                          value=TRUE)
                    ),
                    
                    conditionalPanel(condition = "input.plottype == 'ci'",
                                     shiny::checkboxInput(inputId='plot_deltace', 
                                                          label='Plot the confidence envelope using
                                                          the Delta method?',
                                                          value=TRUE)
                    ),
                    
                    
                    conditionalPanel(condition = "input.plottype == 'ci'",
                                     shiny::checkboxInput(inputId='linesearch', 
                                               label='Run line search algorithm to test the null hypothesis that there
                                               is a linear trend? Will plot a thicker green (delta) or yellow (bootstrap) 
                                               line if found.',
                                               value=FALSE)
                    ),
                    
                    conditionalPanel(condition = "input.plottype == 'ci'",
                                     shiny::checkboxInput(inputId='boot', 
                                                          label='Estimate bootstrapped confidence envelope? Can take a prolonged
                                                          amount of time to estimate (2-30+ minutes, depending on model complexity).',
                                                          value=FALSE)
                    ),
                    
                    #Manual input
                    conditionalPanel(condition = "input.method == 'Manually'",
                                     sliderInput(inputId = "nclass",
                                                 label = "Number of latent classes:",
                                                 min = 1, max = 7, value = 2, step = 1),
                                     
                                     hr(),    
                                     h6('Class 1:'),
                                     numberInputRow(inputId="pi1", label=HTML("&pi;")),
                                     numberInputRow(inputId="alpha1.1", label=HTML("&alpha;<sub>1</sub>")),
                                     numberInputRow(inputId="alpha2.1", label=HTML("&alpha;<sub>2</sub>")),
                                     numberInputRow(inputId="beta21.1", label=HTML("&beta;<sub>21</sub>")),
                                     numberInputRow(inputId="psi11.1", label=HTML("&psi;<sub>11</sub>")),
                                     numberInputRow(inputId="psi22.1", label=HTML("&psi;<sub>22</sub>")),
                                     
                                     conditionalPanel(condition = "input.nclass > 1",
                                                      hr(),
                                                      h6('Class 2:'),
                                                      numberInputRow(inputId="pi2", label=HTML("&pi;")),
                                                      numberInputRow(inputId="alpha1.2", label=HTML("&alpha;<sub>1</sub>")),
                                                      numberInputRow(inputId="alpha2.2", label=HTML("&alpha;<sub>2</sub>")),
                                                      numberInputRow(inputId="beta21.2", label=HTML("&beta;<sub>21</sub>")),
                                                      numberInputRow(inputId="psi11.2", label=HTML("&psi;<sub>11</sub>")),
                                                      numberInputRow(inputId="psi22.2", label=HTML("&psi;<sub>22</sub>"))
                                                      ),
                                     
                                     conditionalPanel(condition = "input.nclass > 2",
                                                      hr(),
                                                      h6('Class 3:'),
                                                      numberInputRow(inputId="pi3", label=HTML("&pi;")),
                                                      numberInputRow(inputId="alpha1.3", label=HTML("&alpha;<sub>1</sub>")),
                                                      numberInputRow(inputId="alpha2.3", label=HTML("&alpha;<sub>2</sub>")),
                                                      numberInputRow(inputId="beta21.3", label=HTML("&beta;<sub>21</sub>")),
                                                      numberInputRow(inputId="psi11.3", label=HTML("&psi;<sub>11</sub>")),
                                                      numberInputRow(inputId="psi22.3", label=HTML("&psi;<sub>22</sub>"))
                                     ),
                                     
                                     conditionalPanel(condition = "input.nclass > 3",
                                                      hr(),
                                                      h6('Class 4:'),
                                                      numberInputRow(inputId="pi4", label=HTML("&pi;")),
                                                      numberInputRow(inputId="alpha1.4", label=HTML("&alpha;<sub>1</sub>")),
                                                      numberInputRow(inputId="alpha2.4", label=HTML("&alpha;<sub>2</sub>")),
                                                      numberInputRow(inputId="beta21.4", label=HTML("&beta;<sub>21</sub>")),
                                                      numberInputRow(inputId="psi11.4", label=HTML("&psi;<sub>11</sub>")),
                                                      numberInputRow(inputId="psi22.4", label=HTML("&psi;<sub>22</sub>"))
                                     ),
                                     
                                     conditionalPanel(condition = "input.nclass > 4",
                                                      hr(),
                                                      h6('Class 5:'),
                                                      numberInputRow(inputId="pi5", label=HTML("&pi;")),
                                                      numberInputRow(inputId="alpha1.5", label=HTML("&alpha;<sub>1</sub>")),
                                                      numberInputRow(inputId="alpha2.5", label=HTML("&alpha;<sub>2</sub>")),
                                                      numberInputRow(inputId="beta21.5", label=HTML("&beta;<sub>21</sub>")),
                                                      numberInputRow(inputId="psi11.5", label=HTML("&psi;<sub>11</sub>")),
                                                      numberInputRow(inputId="psi22.5", label=HTML("&psi;<sub>22</sub>"))
                                     ),
                                     
                                     conditionalPanel(condition = "input.nclass > 5",
                                                      hr(),
                                                      h6('Class 6:'),
                                                      numberInputRow(inputId="pi6", label=HTML("&pi;")),
                                                      numberInputRow(inputId="alpha1.6", label=HTML("&alpha;<sub>1</sub>")),
                                                      numberInputRow(inputId="alpha2.6", label=HTML("&alpha;<sub>2</sub>")),
                                                      numberInputRow(inputId="beta21.6", label=HTML("&beta;<sub>21</sub>")),
                                                      numberInputRow(inputId="psi11.6", label=HTML("&psi;<sub>11</sub>")),
                                                      numberInputRow(inputId="psi22.6", label=HTML("&psi;<sub>22</sub>"))
                                     ),
                                     
                                     conditionalPanel(condition = "input.nclass > 6",
                                                      hr(),
                                                      h6('Class 7:'),
                                                      numberInputRow(inputId="pi7", label=HTML("&pi;")),
                                                      numberInputRow(inputId="alpha1.7", label=HTML("&alpha;<sub>1</sub>")),
                                                      numberInputRow(inputId="alpha2.7", label=HTML("&alpha;<sub>2</sub>")),
                                                      numberInputRow(inputId="beta21.7", label=HTML("&beta;<sub>21</sub>")),
                                                      numberInputRow(inputId="psi11.7", label=HTML("&psi;<sub>11</sub>")),
                                                      numberInputRow(inputId="psi22.7", label=HTML("&psi;<sub>22</sub>"))
                                     )
                                     
                    ),

                    hr(),

                    submitButton(text = "Update")
                    
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
                    original_dir <- getwd()
                    on.exit(setwd(original_dir))
                    if(input$method == 'Manually'){
                        
                        pi <- alpha1 <- alpha2 <- beta21 <- psi11 <- psi22 <- numeric(nclass)
                        if(is.na(input$pi1))
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
                            setwd(input$Mpath)
                            files <- dir()
                            file <- files[grepl("*\\.out$", files)]
                            if(!length(file))
                                return(NULL)
                            if(length(file) > 1L)
                                stop('Multiple .out files in specifed directory')
                            read <- suppressWarnings(MplusAutomation::readModels(file, recursive=TRUE))
                        } else {
                            return(NULL)
                        }
                        if(input$plottype != 'ci'){
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
                        } else {
                            boot <- input$boot
                            setup <- read.plotSEMM_wACOV(read)
                            ret <- plotSEMM_setup2(setup, boot=read, boot.CE=boot, boot.CI=input$plot_bsci,
                                                   alpha = ifelse(input$CI == "95%", .025, .05))
                            if(input$linesearch){
                                lines <- .Call('linear', ret$slo_, ret$shi_, ret$x)
                                line <- which(rowSums(t(ret$slo_<= t(lines)) &
                                                              t(ret$shi_ >= t(lines))) == ncol(lines))
                                if(length(line)){
                                    line <- min(line)
                                    attr(ret, "search") <- rbind(ret$x, lines[line,])
                                }
                                if(boot){
                                    lines <- .Call('linear', ret$bs_lo, ret$bs_high, ret$x)
                                    line <- which(rowSums(t(ret$bs_lo <= t(lines)) &
                                                              t(ret$bs_high >= t(lines))) == ncol(lines))
                                    if(length(line)){
                                        line <- max(line)
                                        attr(ret, "search.bs") <- rbind(ret$x, lines[line,])
                                    }
                                }
                            }
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
                        if(plottype == 'ci') plotSEMM_ci(ret, linesearch=input$linesearch,
                                                         deltaci=input$plot_deltaci,
                                                         bsci=input$plot_bsci,
                                                         deltace=input$plot_deltace,
                                                         ninty_five=input$CI == "95%")
                    } else examplePlot()
                })                
                
            } #end server function
            
        ) #end list collector
        
        return(ret)
}
 