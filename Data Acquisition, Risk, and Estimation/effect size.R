#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

#This app calculates the needed effect size to meet some financial goal.
#It then calculates what that sample size would be given alpha and beta.
#It can handle normal, binomial, and Poisson questions.


library(shiny)
library(ggplot2)
library(lolcat)

options(sci.pen=99,digits = 10)    # Increase digits for precision

#Colors
pal_col<-"R4"#default to a colorblind safe , equal perceptual weight, qualitative palette

color=palette.colors(n = 8,palette = pal_col)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Financial Effect Size Calculation"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "ecalc_dist",label = "Select distribution",choices = c("Normal"=1,"Binomial"=2,"Possion Rate"=3)),
          #numericInput(inputId = "ecalc_mu",label = "Current mean",value = 110.5),
          uiOutput("ecalc_ev"),
          conditionalPanel(condition = "input.ecalc_dist == 1",
                           numericInput(inputId = "ecalc_sig",label = "Current standard deviation",value = 5)
          ),
          numericInput(inputId = "ecalc_loss",label = "Current annual loss",value = 250000),
          #numericInput(inputId = "ecalc_save",label = "Required annual savings",value = 137500),
          numericInput(inputId = "ecalc_cost",label = "Cost of solution",value = 225000),
          numericInput(inputId = "ecalc_roi",label = "ROI Savings Needed (x of cost)",value = 1),
          numericInput(inputId = "ecalc_time",label = "Years to ROI",value = 2),
          conditionalPanel(condition = "input.ecalc_dist == 1",
                           numericInput(inputId = "ecalc_lsl",label = "Lower spec limit",value = 100),
                           numericInput(inputId = "ecalc_usl",label = "Upper spec limit",value = NA)
          ),

          numericInput(inputId = "ecalc_dec",label = "Decimals",value = 3,min = 0,max = 9)
        ),

        # main panel output
        mainPanel(
          conditionalPanel(condition = "input.ecalc_dist == 1",
                           selectInput(inputId = "ecalc_mean_or_sd",label = "Change in mean or standard deviation?",choices = c("Mean"=1,"Standard Deviation"=2))
                           ),
          plotOutput("ecalc_plot"),
          htmlOutput("ecalc_out"),
          h2("Sample Size"),
          conditionalPanel(condition = "input.ecalc_dist == 2 || input.ecalc_dist ==3 || (input.ecalc_dist==1 && input.ecalc_mean_or_sd == 2)",
                           p("For asymmetrical random sampling distributions the sample size shown will be the higher possibility.")
                           ),
          fluidRow(
            column(4,
                   selectInput(inputId = "ecalc_alpha",label = "\U03B1",choices = c(.1,.05,.01))
                   ),
            column(4,
                   selectInput(inputId = "ecalc_beta",label = "\U03B2",choices = c(.1,.05,.01))
            ),
            column(4,
                   selectInput(inputId = "ecalc_hyp",label="Choose alternative",choices = c("Two-tailed"="two.sided","One-tailed"="less"))
                   )
          ),
          
          verbatimTextOutput("ecalc_ssize")
        )
    )
)

# Define server logic
server <- function(input, output) {
  
    #global reactive value
    rvalues<-reactiveValues(new_mu=0,new_sd=0,new_max_pct_out=0,n=0)
  
  #responsive UI
  output$ecalc_ev<-renderUI({
    dist<-input$ecalc_dist
    
    if(dist==1){
      numericInput(inputId = "ecalc_mu",label = "Current \U03BC",value = 110.5)
    } else if(dist==2){
      numericInput(inputId = "ecalc_mu",label = "Current \U03C0",value = 0.06687,min = 0,max = 1)
    }else if(dist==3){
      numericInput(inputId = "ecalc_mu",label = "Current \U03BB",value = 110.5)
    }
  })

  output$ecalc_out<-renderUI({
    dist<-input$ecalc_dist
    mu<-input$ecalc_mu
    sig<-input$ecalc_sig
    loss <-input$ecalc_loss
    #save<-input$ecalc_save
    cost<-input$ecalc_cost
    lsl<-input$ecalc_lsl
    usl<-input$ecalc_usl
    R<-input$ecalc_dec
    mean_or_sd<-input$ecalc_mean_or_sd
    roi<-input$ecalc_roi
    roi_time<-input$ecalc_time
    
    req(dist,mu)#,loss,save)
    
    roi_req=roi*cost/roi_time
    
    save=loss-roi_req
    
    if(dist==1){ #Normal
      req(sig,loss,cost,R,mean_or_sd,roi,roi_time)
      if(!isTruthy(usl) && !isTruthy(lsl)){return()}
      
      out_u<-0
      out_l<-0
      
      if(isTruthy(lsl)){#lsl present
        out_l<-pnorm(q = lsl,mean = mu,sd = sig,lower.tail = TRUE)
      }
      if(isTruthy(usl)){#usl present
        out_u<-pnorm(q = usl,mean = mu,sd = sig,lower.tail = FALSE)
      }
      total_out<-out_l+out_u
      loss_per_pct<-loss/(total_out*100)
      rvalues$new_max_pct_out<-save/loss_per_pct
      
      
        if(isTruthy(lsl) && isTruthy(usl)){#two specs
          if(mean_or_sd==1 && (pnorm(q = lsl,mean = mean(c(usl,lsl)),sd = sig,lower.tail = TRUE)+
             pnorm(q = usl,mean = mean(c(usl,lsl)),sd = sig,lower.tail = FALSE))>(rvalues$new_max_pct_out/100)){#can't fit the spec
            rvalues$new_mu<-(mean(c(usl,lsl)))
            return(HTML("Impossible to shift mean to get the target reduction in nonconformance, try reducing the standard deviation"))
          }
          new_z_l<-qnorm(rvalues$new_max_pct_out/100,lower.tail = TRUE)
          new_mu_l <- -(new_z_l*sig - lsl)
          delta_l<-mu-new_mu_l
          new_z_u<-qnorm(rvalues$new_max_pct_out/100,lower.tail = FALSE)
          new_mu_u <- -(new_z_u*sig - usl)
          delta_u<-mu-new_mu_u
          
          results<-data.frame(new_mu=c(new_mu_l,new_mu_u),delta=c(delta_l,delta_u),new_z=c(new_z_l,new_z_u))

          if(all.equal(abs(results$delta[1]),abs(results$delta[2])) == TRUE){#delta is same, report both
            rvalues$new_mu<-results$new_mu
            new_z<-results$new_z
            delta<-results$delta
          } else{#delta is different, report larger delta
            new_mu<-(results[which.max(results$delta),]$new_mu)
            new_z<-results[which.max(results$delta),]$new_z
            delta<-results[which.max(results$delta),]$delta
          }

        } else if(isTruthy(lsl)){#lower spec only
          new_z<-qnorm(rvalues$new_max_pct_out/100,lower.tail = TRUE)
          rvalues$new_mu <- (-(new_z*sig - lsl))
          delta<-rvalues$new_mu-mu
        } else if(isTruthy(usl)){#upper spec only
          new_z<-qnorm(rvalues$new_max_pct_out/100,lower.tail = FALSE)
          rvalues$new_mu<- (-(new_z*sig - usl))
          delta<-rvalues$new_mu-mu
        }
      if(mean_or_sd==1){#try to move mean
        rvalues$new_sd<-sig
        } else {#move sd
          if(isTruthy(lsl && isTruthy(usl))){#not right yet
            new_sd_l <- (mu - lsl)/abs(new_z)
            new_sd_u <- (usl - mu)/abs(new_z)
            rvalues$new_sd<-min(new_sd_u,new_sd_l)
          } else if(isTruthy(lsl)){
            rvalues$new_sd <- (mu - lsl)/abs(new_z)
          } else if(isTruthy(usl)){
            rvalues$new_sd <- (usl - mu)/abs(new_z)
          }
         delta<-0
         
        }
      output<-HTML(paste0("Current \U03BC = ",round(x = mu,digits = R),"<br>"
                        ,"Current \U03C3 = ",round(x = sig,digits = R),"<br>"
                        ,"Out low = ",round(x = 100*out_l,digits = R),"%<br>"
                        ,"Out high = ",round(100*out_u,R),"%<br>"
                        ,"Total % Out = ",round(100*total_out,R),"%<br>"
                        ,"Loss per % = ",round(loss_per_pct,R),"<br>"
                        ,"Annual ROI required = ",round(roi_req,R)," over ",roi_time," years<br>"
                        ,"Max Annual Loss = ",round(save,R),"<br>"
                        ,"New Max % Out = ",round(rvalues$new_max_pct_out,R),"%<br>"
                        ,"New z = ",paste0(round(new_z,R),collapse=", "),"<br>"
                        ,"New \U03BC = ",paste0(round(rvalues$new_mu,R),collapse=", "),"<br>"
                        ,"New standard deviation = ",round(rvalues$new_sd,R),"<br>"
                        ,"Delta = ",paste0(round((delta),R),collapse = ", ")
                        ))
      
    } else if (dist==2){
      req(loss,cost,R,mean_or_sd,roi,roi_time)
      req(mu<1 && mu>0)#make into message
      
      loss_per_pct<-loss/(mu*100)
      rvalues$new_max_pct_out<-save/loss_per_pct
      
        output<-HTML(paste0("Current \U03C0 = ",round(100*mu,R),"%<br>"
                            ,"Loss per % = ",round(loss_per_pct,R),"<br>"
                            ,"Annual ROI required = ",round(roi_req,R)," over ",roi_time," years<br>"
                            ,"Max Annual Loss = ",round(save,R),"<br>"
                            ,"New \U03C0 = ",round(rvalues$new_max_pct_out,R),"%<br>"
        ))
      
    } else if (dist==3){
      req(loss,cost,R,mean_or_sd,roi,roi_time)
      
      loss_per_pct<-loss/(mu)
      rvalues$new_max_pct_out<-save/loss_per_pct
      
      output<-HTML(paste0("Current \U03BB = ",round(mu,R),"<br>"
                          ,"Loss per count = ",round(loss_per_pct,R),"<br>"
                          ,"Annual ROI required = ",round(roi_req,R)," over ",roi_time," years<br>"
                          ,"Max Annual Loss = ",round(save,R),"<br>"
                          ,"New \U03BB = ",round(rvalues$new_max_pct_out,R),"<br>"
      ))
      
    }
    
    
    
    output
  })
  
  output$ecalc_plot<-renderPlot({
    dist<-input$ecalc_dist
    mu<-input$ecalc_mu
    sig<-input$ecalc_sig
    lsl<-input$ecalc_lsl
    usl<-input$ecalc_usl
    
    req(dist,mu)
    
    if(dist==1){#normal
      mean_or_sd<-input$ecalc_mean_or_sd
      req(rvalues$new_mu,rvalues$new_sd)
      
      xmin<-min(
        mu-6*sig,
        rvalues$new_mu-6*sig
      )
      if(isTruthy(lsl)){
        xmin<-min(xmin,lsl)
      }
      xmax<-max(
        mu+6*sig,
        rvalues$new_mu+6*sig
      )
      if(isTruthy(usl)){
        xmax<-max(xmax,usl)
      }
      
      p<-ggplot()+
        xlim(xmin,xmax)+
        stat_function(fun = dnorm,args = list(mean=mu,sd=sig),color=color[1])
      if(mean_or_sd==2){
        p<-p+
          stat_function(fun = dnorm,args = list(mean=mu,sd=rvalues$new_sd),color=color[4])
      } else{
        p<-p+
          stat_function(fun = dnorm,args = list(mean=rvalues$new_mu,sd=rvalues$new_sd),color=color[4])
      }
      
      if(isTruthy(lsl)){
        p<-p+geom_vline(aes(xintercept=lsl),linetype=5,color=color[2])
      }
      if(isTruthy(usl)){
        p<-p+geom_vline(aes(xintercept=usl),linetype=5,color=color[2])
      }
      
    } else if(dist==2){#binomial
      n<-rvalues$n
      req(rvalues$new_max_pct_out,n>0,mu<1)
      
      xmin<-max(0,
                min(
                  as.integer(n*mu-6*(n*mu)^.5),
                  as.integer(n*.01*rvalues$new_max_pct_out-6*(n*.01*rvalues$new_max_pct_out)^.5))
      )
      xmax<-min(n,as.integer(n*mu+6*(n*mu)^.5))
      
      x<-seq(xmin,xmax,by=1)
      y<-dbinom(x=x,prob = mu,size = n)
      y2<-dbinom(x=x,prob = .01*rvalues$new_max_pct_out,size = n)
      plot_data<-data.frame(x,y,y2)
      
      
      p<-ggplot(data=plot_data,aes(x=x,y=y))+
        geom_col(color=color[1],fill=color[2])+
        geom_point(aes(y=y2),color=color[4])+
        geom_segment(aes(yend=y2,xend=x),color=color[4])
      
    }else if (dist==3){#Poisson
      req(rvalues$new_max_pct_out)
      
      xmin<-max(0,
                min(
                as.integer(mu-6*(mu)^.5),
                as.integer(rvalues$new_max_pct_out-6*(rvalues$new_max_pct_out)^.5))
                )
      xmax<-as.integer(mu+6*(mu)^.5)
      
      x<-seq(xmin,xmax,by=1)
      y<-dpois(x=x,lambda = mu)
      y2<-dpois(x=x,lambda = rvalues$new_max_pct_out)
      plot_data<-data.frame(x,y,y2)
      
      
      p<-ggplot(data=plot_data,aes(x=x,y=y))+
        geom_col(color=color[1],fill=color[2])+
        geom_point(aes(y=y2),color=color[4])+
        geom_segment(aes(yend=y2,xend=x),color=color[4])

    }
    
    p
    
  })
  
  output$ecalc_ssize<-renderPrint({
    dist<-input$ecalc_dist
    alpha<-as.numeric(input$ecalc_alpha)
    beta<-as.numeric(input$ecalc_beta)
    hyp<-input$ecalc_hyp
    mu<-input$ecalc_mu
    sig<-input$ecalc_sig
    mean_or_sd<-input$ecalc_mean_or_sd
    req(dist,alpha,beta,hyp,mu)

    if(dist==1){
      if(mean_or_sd==1){#move mean
        req(sig,mean_or_sd)
        noquote(t(sample.size.mean.t.onesample(effect.size = -abs(rvalues$new_mu-mu),variance.est = sig^2,alpha = alpha,beta = beta,alternative = hyp)))
      } else{#reduce sd
        req(sig,rvalues$new_sd)
        if(hyp=="less"){
          noquote(t(sample.size.variance.onesample(null.hypothesis.variance = sig^2,alternative.hypothesis.variance = rvalues$new_sd^2,alpha = alpha,beta = beta,alternative = hyp,details = TRUE,power.from.actual = TRUE)))
        } else{#two-tailed, consider both sides sample sizes
          #less
           temp1<-sample.size.variance.onesample(null.hypothesis.variance = sig^2,alternative.hypothesis.variance = rvalues$new_sd^2,alpha = alpha,beta = beta,alternative = hyp,details = TRUE,power.from.actual = TRUE)
           #more
           temp2<-sample.size.variance.onesample(null.hypothesis.variance = sig^2,alternative.hypothesis.variance = (sig+sig-rvalues$new_sd)^2,alpha = alpha,beta = beta,alternative = hyp,details = TRUE,power.from.actual = TRUE)
           if(temp2$sample.size >= temp1$sample.size){
             noquote(t(temp2))
           } else{
             noquote(t(temp1))
           }
        }
        
      }


    } else if(dist==2){
      req(rvalues$new_max_pct_out,mu<1)
      if(hyp=="less"){
        noquote(t(sample.size.proportion.test.onesample.exact(null.hypothesis.proportion = mu,alternative.hypothesis.proportion = rvalues$new_max_pct_out/100,alpha = alpha,beta = beta,alternative = hyp)))
      } else{
        temp1<-sample.size.proportion.test.onesample.exact(null.hypothesis.proportion = mu,alternative.hypothesis.proportion = rvalues$new_max_pct_out/100,alpha = alpha,beta = beta,alternative = hyp)
        temp2<-sample.size.proportion.test.onesample.exact(null.hypothesis.proportion = mu,alternative.hypothesis.proportion = mu+mu-rvalues$new_max_pct_out/100,alpha = alpha,beta = beta,alternative = hyp)
        if(temp2$sample.size >= temp1$sample.size){
          rvalues$n<-temp2$sample.size
          noquote(t(temp2))
        } else{
          rvalues$n<-temp2$sample.size
          noquote(t(temp1))
        }
      }
      

    }else if (dist==3){#fix error when two.tailed
      if(hyp=="less"){
        noquote(t(sample.size.count.poisson.onesample.exact(lambda.null.hypothesis = mu,lambda.alternative.hypothesis = rvalues$new_max_pct_out,alpha = alpha,beta = beta,alternative = hyp)))
      } else{
        temp1<-sample.size.count.poisson.onesample.exact(lambda.null.hypothesis = mu,lambda.alternative.hypothesis = rvalues$new_max_pct_out,alpha = alpha,beta = beta,alternative = hyp)
        temp2<-sample.size.count.poisson.onesample.exact(lambda.null.hypothesis = mu,lambda.alternative.hypothesis = mu+mu-rvalues$new_max_pct_out,alpha = alpha,beta = beta,alternative = hyp)
        if(temp2$sample.size >= temp1$sample.size){
          noquote(t(temp2))
        } else{
          noquote(t(temp1))
        }
        
      }

    }
    
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
