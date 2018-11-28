## Script setup

  library(ggrepel)
  library(grid)
  library(openxlsx)
  library(shiny)
  library(ggthemes)
  library(lubridate)
  library(shinythemes)
  library(clipr)
  library(dplyr)
  library(stringr)


  # Define server logic to read selected file ----
  server <- function(input, output) {

    
    # input$file1 will be NULL initially
    #output$JK_data_upload <- renderTable(values$JK_data)

    output$JK_data_upload <- renderTable({
      
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      
      req(input$file1)
      
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
        {
          JK_data <- read.csv(input$file1$datapath,
                         header = input$header,
                         sep = input$sep,
                         dec = ",",
                         stringsAsFactors = FALSE)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
      
      colnames(JK_data)<-c("No","Naam","Downtime","Datum")
      
      JK_data$Compleet<-"Ja"
      JK_data$Datum<-as.Date(JK_data$Datum,format="%d-%m-%Y")
      JK_data$Jaar<-format(year(JK_data$Datum),nsmall=0)
      JK_data$Week<-paste(JK_data$Jaar,format(week(JK_data$Datum),nsmall=0),sep="-")
      JK_data$Maand<-paste(JK_data$Jaar,format(month(JK_data$Datum),nsmall=0),sep="-")
      JK_data$Kwartaal<-paste(JK_data$Jaar,format(quarter(JK_data$Datum),nsmall=0),sep="-")

      str(JK_data)
      
      JK_data<<-JK_data
      JK_data
    })
    
    # Data samenvatting----
    output$samenvatting <- renderTable({

      req(input$file1)
      
      filter_naam<-"Naam"

      data<-JK_data%>%filter(Datum>=input$Begindatum,Datum<=input$Einddatum)
      
      JK_data_tr<-data%>% group_by_(filter_naam,input$JK_interval) %>% 
        summarise(Tot_downtime = sum(Downtime),Storingen=length(Naam), MTTR=mean(Downtime))
      
      })
    
    # Jackknife diagram berekenen
    plotInput <- reactive({

      req(input$file1)
      
      filter_naam<-"Naam"
      
      data<-JK_data%>%filter(Datum>=input$Begindatum,Datum<=input$Einddatum)
      
      JK_data_tr<-data  %>% group_by_(filter_naam,input$JK_interval) %>% 
        summarise(Tot_downtime = sum(Downtime),Storingen=length(Naam), MTTR=mean(Downtime))
      
      ## Standaardwaardes
      
      MTBF_max<-ceiling(max(JK_data_tr$Storingen))
      MTBF_mean<-ceiling(mean(JK_data_tr$Storingen))
      
      uu<-seq(1,10,1)
      qq<-ceiling(log10(MTBF_max))+1
      tt<-uu[uu<=qq]
      dd<-c(1:9 %o% 10^tt)/(10^ceiling(log10(MTBF_max)))

      MTBF_axis<-c(dd,10^(qq-1))
      MTBF_limits<-c(0.1,10^ceiling(log10(MTBF_max)))
      
      MTTR_max<-ceiling(max(JK_data_tr$MTTR))
      MTTR_mean<-ceiling(mean(JK_data_tr$MTTR))
      
      uu<-seq(1,10,1)
      qq<-ceiling(log10(MTTR_max))+1
      tt<-uu[uu<=qq]
      dd<-c(1:9 %o% 10^tt)/(10^ceiling(log10(MTTR_max)))
      n
      MTTR_axis<-c(dd,10^(qq-1))
      MTTR_limits<-c(0.1,10^ceiling(log10(MTTR_max)))
      
      ## Plot

      label_interval<-paste("JK_data_tr$",input$JK_interval,sep="")
      
      jk_plot<<-ggplot(data=JK_data_tr,aes(Storingen,MTTR,label=Naam,
      group=Naam))+ geom_point()+
      geom_line(arrow = arrow(length=unit(0.30,"cm"), type = "closed"),aes(color=Storingen*MTTR))+
      scale_y_log10(limits=MTTR_limits,breaks=MTTR_axis)+
      scale_x_log10(limits=MTBF_limits,breaks=MTBF_axis)+
      geom_text_repel(aes(color=Storingen*MTTR)) +
      geom_hline(yintercept=MTTR_mean,color="red")+
      geom_vline(xintercept=sum(JK_data_tr$Storingen)/length(JK_data_tr$Storingen),color="red")+
      ggtitle(paste("\n Jackknife diagram (trend obv ",input$JK_interval,"), gemaakt op ",
      Sys.Date(),"\n",sep=""))+
      annotate("text", 
          x = c(min(MTBF_limits)*1.2,min(MTBF_limits)*1.2,max(MTBF_limits)*0.9,max(MTBF_limits)*0.9),
          y = c(min(MTTR_limits),max(MTTR_limits),min(MTTR_limits),max(MTTR_limits)), 
          label = c("www.CChange.nl", "Accuut (MTTR)","Chronisch (MTBF)","Accuut & Chronisch"),
          color="royalblue")+
      scale_color_gradient(low = "green",high = "red",name= "Prioriteit")+
      theme_bw()+
      theme(plot.title=element_text(size=18)) 

      jk_plot
      
    })
    
    # Jackknife diagram plotten
    output$plot_jk_tr <- renderPlot({
      print(plotInput())
    })
    
    # Template uploadsheet downloaden ----
    output$Download_Uploadsheet_Jackknife.csv<- downloadHandler(
      filename = "Uploadsheet_Jackknife.csv",
      content = function(file) {
        uploadsheet<-as.data.frame(matrix(nrow=1,ncol=4))
        colnames(uploadsheet)=c("Storingsno.","ComponentNaam","DowntimePerStoring","DatumStoring")
        str(uploadsheet)
        write.csv(uploadsheet,file,row.names = FALSE)
    })
    
    # Jackknife downloaden ----
    output$Jackknife.png <- downloadHandler(
      filename = "Jackknife.png",
      content = function(file) {
        ggsave(file, plot = plotInput(), device = "png",scale=4,width = 10, height = 5, units = "cm")
      }
    )
  }
  