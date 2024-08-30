#Testing alpha, beta, sample size, power, delta

library(shiny)
library(ggplot2)

#Colors
pal_col<-"R4"#default to a colorblind safe , equal perceptual weight, qualitative palette

color=palette.colors(n = 8,palette = pal_col)

hyp_choices<-c(
  "H\U2081: \U03BC \U2260 0"=1,
  "H\U2081: \U03BC < 0"=2,
  "H\U2081: \U03BC > 0"=3
)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Testing \U03B1, \U03B2, \U03C3, sample size, \U0394"),
  
    withMathJax(),
  tags$div(HTML("<script type='text/x-mathjax-config'>
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                });
                </script>
                ")),

    # Application title

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          numericInput(inputId = "pop_sd",label = "Population \U03C3",value = 4),
          sliderInput(inputId = "sample_n",label = "Sample Size (n)",value = 1,min = 1,max = 30),
          selectInput(inputId = "alpha",label = "\U03B1",choices = c(0.1,0.05,0.01)),
          sliderInput(inputId = "delta",label = "Effect Size (\U0394)",value = 1,min = 1,max = 30),
          selectInput(inputId = "hyp",label = "Select Alternative Hypothesis",choices = hyp_choices),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("graph",width="100%",height="600px"),
           htmlOutput("stand_err")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$stand_err<-renderUI({#output helpful info
    sd<-input$pop_sd
    n<-input$sample_n
    alpha<-as.numeric(input$alpha)
    delta<-input$delta
    hyp<-input$hyp
    
    sd_error<-round(sd/sqrt(n),3)
    
    if(hyp==1){
      alpha_rsd<-alpha/2
    } else {
      alpha_rsd<-alpha
    }
    
    #critical values
    crit_u<-qnorm(p = alpha_rsd,mean = 0,sd = sd_error,lower.tail = FALSE)
    
    beta<-round(pnorm(q = crit_u,mean = delta,sd = sd_error,lower.tail = TRUE),3)#calculation is the same for either alternate hyp
    
      div(HTML(paste0(
        withMathJax(sprintf("$\\sigma_{\\overline{X}}=\\frac{\\sigma_{X}}{\\sqrt{n}}=$")),sd_error,
        "<br>",
        withMathJax(sprintf("$\\beta=$")),beta,"<br> Power = ",1-beta
        )
        ),style="font-size: 30px; text-align: center;")

  })
  
  output$graph<-renderPlot({
      sd<-input$pop_sd
      n<-input$sample_n
      alpha<-as.numeric(input$alpha)
      delta<-input$delta
      hyp<-input$hyp
      
      sd_error<-sd/sqrt(n)
      
      if(hyp==1){
        alpha_rsd<-alpha/2
      } else {
        alpha_rsd<-alpha
      }
      
      xmin<-min(0-4*sd,0-delta-4*(sd/sqrt(n)))
      xmax<-max(0+4*sd,0+delta+4*(sd/sqrt(n)))
      
      #critical values
      crit_u<-qnorm(p = alpha_rsd,mean = 0,sd = sd_error,lower.tail = FALSE)
      crit_l<-qnorm(p = alpha_rsd,mean = 0,sd = sd_error,lower.tail = TRUE)
      
      slices=500#how many points across all distributions
      
      x<-seq(from=xmin, to=xmax, length.out=slices)
      
      #bhd
      bhd<-data.frame(facet=1,plot="bhd",x=x,y=dnorm(x = x,mean = 0,sd = sd))
      annotate<-data.frame(facet=c(1,2,3),x=0,y=0,label=c("\U03BC","",""),vjust=0,hjust=.5)
      
      #null RSD
      null_rsd<-data.frame(facet=2,plot="null_base",x=x,y=dnorm(x = x,mean = 0,sd = sd_error))
      plot_data<-rbind(bhd,null_rsd)
      annotate<-rbind(annotate,data.frame(facet=2,x=0,y=.5*dnorm(x = 0,mean = 0,sd = sd_error),label="Confidence",vjust=0,hjust=.5))
      
      
      #null upper tail
      if(hyp==1 || hyp==3){#need an upper
        null_upper<-data.frame(facet=2,plot="null_upper",x=x,y=dnorm(x = x,mean = 0,sd = sd_error))
        null_upper$y[null_upper$x<crit_u]<-0
        plot_data<-rbind(plot_data,null_upper)
        if(hyp==1){
          annotate<-rbind(annotate,data.frame(facet=2,x=crit_u,y=dnorm(x = crit_u,mean = 0,sd = sd_error),label="\U03B1/2",vjust=0,hjust=-1))
        } else{
          annotate<-rbind(annotate,data.frame(facet=2,x=crit_u,y=dnorm(x = crit_u,mean = 0,sd = sd_error),label="\U03B1",vjust=0,hjust=-1))
        }
      }
      #null lower tail
      if(hyp==1 || hyp==2){#need a lower
        null_lower<-data.frame(facet=2,plot="null_lower",x=x,y=dnorm(x = x,mean = 0,sd = sd_error))
        null_lower$y[null_lower$x>crit_l]<-0
        plot_data<-rbind(plot_data,null_lower)
        if(hyp==1){
          annotate<-rbind(annotate,data.frame(facet=2,x=crit_l,y=dnorm(x = crit_u,mean = 0,sd = sd_error),label="\U03B1/2",vjust=0,hjust=2))
        } else{
          annotate<-rbind(annotate,data.frame(facet=2,x=crit_l,y=dnorm(x = crit_u,mean = 0,sd = sd_error),label="\U03B1",vjust=0,hjust=2))
        }
      }
      
      #alt RSD
      #need upper alt
      if(hyp==1 || hyp ==3){
        alt_rsd2<-data.frame(facet=3,plot="alt2",x=x,y=dnorm(x = x,mean = 0+delta,sd = sd_error))
        alt_rsd2_tail<-data.frame(facet=3,plot="alt2_tail",x=x,y=dnorm(x = x,mean = 0+delta,sd = sd_error))
        alt_rsd2$y[alt_rsd2$x<crit_u]<-0
        alt_rsd2_tail$y[alt_rsd2_tail$x>crit_u]<-0
        plot_data<-rbind(plot_data,alt_rsd2,alt_rsd2_tail)
        annotate<-rbind(annotate,data.frame(facet=3,x=crit_u,y=.5*dnorm(x = crit_u,mean = delta,sd = sd_error),label="\U03B2",vjust=0,hjust=2))
        annotate<-rbind(annotate,data.frame(facet=3,x=crit_u,y=.5*dnorm(x = crit_u,mean = 0,sd = sd_error),label="Power",vjust=0,hjust=-1))
      }
      
      #need lower alt
      if(hyp==1 || hyp ==2){
      alt_rsd1<-data.frame(facet=3,plot="alt1",x=x,y=dnorm(x = x,mean = 0-delta,sd = sd_error))
      alt_rsd1_tail<-data.frame(facet=3,plot="alt1_tail",x=x,y=dnorm(x = x,mean = 0-delta,sd = sd_error))
      alt_rsd1$y[alt_rsd1$x>crit_l]<-0
      alt_rsd1_tail$y[alt_rsd1_tail$x<crit_l]<-0
      plot_data<-rbind(plot_data,alt_rsd1,alt_rsd1_tail)
      annotate<-rbind(annotate,data.frame(facet=3,x=crit_l,y=.5*dnorm(x = crit_l,mean = -delta,sd = sd_error),label="\U03B2",vjust=0,hjust=-2))
      annotate<-rbind(annotate,data.frame(facet=3,x=crit_l,y=.5*dnorm(x = crit_u,mean = 0,sd = sd_error),label="Power",vjust=0,hjust=2))
      }
      
      #create factor for the three plots
      plot_data$facet<-factor(x = plot_data$facet,levels = c(1,2,3),labels = c("Population","Null RSD","Alternative RSD"))
      annotate$facet<-factor(x = annotate$facet,levels = c(1,2,3),labels = c("Population","Null RSD","Alternative RSD"))
      
      #custom colors
      leg_names<-c("bhd"=alpha(color[4],.5),
                   "null_base"=alpha(color[5],.5),
                   "null_lower"=color[2],
                   "null_upper"=color[2],
                   "alt2_tail"=alpha(color[7],.5),
                   "alt1_tail"=alpha(color[7],.5),
                   "alt1"=alpha(color[3],.5),
                   "alt2"=alpha(color[3],.5)
                   )
      
      p<-ggplot(data=plot_data,aes(x=x,y=y,fill=plot))+
        facet_grid(rows=vars(facet),scales="free_y")+
        geom_line()+
        geom_area(position = "identity")+ #geom_area stacks by default
        theme(axis.title = element_text(size=rel(1.5)),
              axis.text = element_text(size=rel(1.5)),
              plot.title = element_text(size=rel(1.5)),
              strip.text = element_text(size=rel(1.5))
              ,legend.position = "none"
              )+
        scale_fill_manual(values=leg_names)+
        ylab("Density")+
       geom_text(data=annotate,mapping=aes(x=x,y=y,label=label,vjust=vjust,hjust=hjust),inherit.aes = FALSE,size=rel(5))#labels
      
      p
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
