#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Librerias ----
if(require(tidyverse)==FALSE){install.packages("tidyverse")}
library(tidyverse)
library(shiny)
library(circlize)
library(png)

## Lectura Inputs
dataBase=read.table(file = "Data/Climatologicos_1980_2010_pr.csv",sep = ",",header = T,dec = ".",stringsAsFactors = T) 
Lis_Est=read.table("Data/Listado_Mun_Colom_Estaciones_IDEAM_Climatologia.csv",sep = ";",header = T)
regimen_PPt0=read.table("Data/regimenes_departamentos.csv",sep = ";",dec = ".",header = T)

files = list.files("Data/icon_calen_clim/png/", full.names = TRUE)

# Definiciones ----
Lis_Est=subset(Lis_Est,Lis_Est$CODIGO !="-999")
##Arreglar nombres de mayus a MayusMinus xra unir a lsitado de est disponbles con climatologia
Nombre_Dep2=stringi::stri_trans_tolower(Lis_Est$Nombre_Dep)
Lis_Est$Nombre_Dep2=Nombre_Dep2
#Municipio=stringr::str_to_title(Lis_Est$Municipio)
Municipio=stringi::stri_trans_tolower(Lis_Est$Municipio)
Lis_Est$Municipio2=Municipio

zona2=stringi::stri_trans_tolower(regimen_PPt0$Zona)
regimen_PPt0$Zona2=zona2

Lis_Est_Menu <- paste(Lis_Est$Municipio2,Lis_Est$Nombre_Dep2,sep = ",")
Lis_Est_Menu_iest <- seq(1,length(Lis_Est_Menu))
Menu_Est <- paste(Lis_Est_Menu,"=",Lis_Est_Menu_iest)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Calendario Climático"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30),
            
            selectInput(inputId = "estMun",label = "Municipio",choices = Lis_Est_Menu,selected = "espinal,tolima" )
            #selectInput(inputId = "iest",label = "Municipio",choices = c(Menu_Est))
            #selectInput(inputId = "iest",label = "Municipio",choices = c(as.character(Lis_Est_Menu) = as.character(Lis_Est_Menu_iest)))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           #plotOutput("distPlot"),
           
           #textOutput("prueba"),
           plotOutput("CalPlot")
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    
    output$prueba <- renderText({
      #input$estMun
      #str_split(input$estMun,",", simplify = TRUE)[1]
      iest <- which(Lis_Est_Menu == input$estMun)
      Lis_Est$Municipio2[iest]

    })
    
    
    output$CalPlot <- renderPlot({
      
      iest <- which(Lis_Est_Menu == input$estMun)
      
      if (Lis_Est$Tien_clim[iest]==1) {
        
        codi=Lis_Est$CODIGO[iest]
        vals_ppt=dataBase[dataBase$CODIGO==codi&dataBase$VARIABLE=="ppt",9:20]
        vals_No_ppt=dataBase[dataBase$CODIGO==codi&dataBase$VARIABLE=="No_ppt",9:20]
        Depa=Lis_Est$Nombre_Dep2[iest]
        Muni=Lis_Est$Municipio2[iest]
        print(paste(codi,Depa,Muni,sep = " "))
        
        x=seq(1,12)
        files = list.files("Data/icon_calen_clim/png/", full.names = TRUE)
        PathIcons=("Data/icon_calen_clim/png/")
        meses=colnames(dataBase[9:20])
        #meses=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
        temporada_PPt=c("sol","sol","trs","ppt","ppt","trs","sol","sol","trs","ppt","ppt","trs")
        coloRGB=rgb(c(17,18,255),c(48,103,176),c(92,114,0),maxColorValue = 255)
        
        regimen_PPt=subset(regimen_PPt0[2:13],regimen_PPt0$Zona2==as.character(Lis_Est$Nombre_Dep2[iest]))
        colo=temporada_PPt # para trampa a R
        colotext=temporada_PPt # para trampa a R
        image=temporada_PPt # para trampa a R
        for(i in seq(1,12)){
          
          if(regimen_PPt[i]==1){colo[i]=coloRGB[1];colotext[i]="white";image[i]=paste(PathIcons,"tempo_ppt1_white.png",sep = "")}
          if(regimen_PPt[i]==0.3){colo[i]=coloRGB[2];colotext[i]="white";image[i]=paste(PathIcons,"trans_sec2ppt_white.png",sep = "")}
          if(regimen_PPt[i]==0.6){colo[i]=coloRGB[2];colotext[i]="white";image[i]=paste(PathIcons,"trans_ppt2sec_white.png",sep = "")}
          if(regimen_PPt[i]==0){colo[i]=coloRGB[3];colotext[i]="black";image[i]=paste(PathIcons,"tempo_sec.png",sep = "")}
        }
        
        #nom_graf=paste("calend_clima_web/",Lis_Est$Nombre_Dep2[iest],"_",Lis_Est$Municipio2[iest],"_","climatologia.png",sep = "")
        #nom_graf=paste("calend_clima_web/",Lis_Est$Nombre_Dep2[iest],"_",Lis_Est$Municipio2[iest],"_",Lis_Est$CODIGO[iest],"_","climatologia.png",sep = "")
        
        circos.par(start.degree = 90)
        circos.initialize(factors = meses,x = x,xlim = c(1,12))
        
        ##Nom Tempo
        #temporada_PPt=c("seco","seco","transic.","lluvioso","lluvioso","transic."," - lluvioso","- lluvioso","transic.","lluvioso","lluvioso","transic")
        circos.trackPlotRegion(bg.col = colo,ylim = c(0, 1),track.height = 0.08, panel.fun = function(x, y) {
          xcenter = get.cell.meta.data("xcenter")
          ycenter = get.cell.meta.data("ycenter")
          pos = circlize:::polar2Cartesian(circlize(xcenter, ycenter))
          circos.text(CELL_META$xcenter, CELL_META$ycenter, 
                      paste(vals_No_ppt[CELL_META$sector.numeric.index]," días lluvia",sep = ""),
                      facing = "bending.inside", niceFacing = TRUE,col = colotext[CELL_META$sector.numeric.index])
        })
        
        ##ppt_values
        circos.trackPlotRegion(bg.col = colo,ylim = c(0, 1),track.height = 0.08, panel.fun = function(x, y) {
          xcenter = get.cell.meta.data("xcenter")
          ycenter = get.cell.meta.data("ycenter")
          pos = circlize:::polar2Cartesian(circlize(xcenter, ycenter))
          circos.text(CELL_META$xcenter, CELL_META$ycenter, 
                      paste(vals_ppt[CELL_META$sector.numeric.index],"mm",sep = ""),
                      facing = "bending.inside", niceFacing = TRUE,col = colotext[CELL_META$sector.numeric.index])
        })
        
        ##tmp_values
        
        #circos.trackPlotRegion(bg.col = colo,ylim = c(0, 1),track.height = 0.08, panel.fun = function(x, y) {
        #  xcenter = get.cell.meta.data("xcenter")
        #  ycenter = get.cell.meta.data("ycenter")
        #  pos = circlize:::polar2Cartesian(circlize(xcenter, ycenter))
        #  circos.text(CELL_META$xcenter, CELL_META$ycenter, 
        #              paste(ejm_tmp[CELL_META$sector.numeric.index],"°C",sep = ""),
        #              facing = "inside", niceFacing = TRUE,col = "white")
        #})
        
        ## Iconos
        circos.trackPlotRegion(bg.col = colo,ylim = c(0, 1),track.height = 0.4, panel.fun = function(x, y) {
          
          #  print(c("pepe",CELL_META$sector.numeric.index))
          iconos = as.raster(readPNG(image[CELL_META$sector.numeric.index]))
          circos.raster(iconos,CELL_META$xcenter, CELL_META$ycenter + 0.1,
                        facing = "downward", niceFacing = TRUE,width = "1.7cm")  
        })
        
        ##Nom Meses
        circos.trackPlotRegion(bg.col = colo,ylim = c(0, 1), panel.fun = function(x, y) {
          xcenter = get.cell.meta.data("xcenter")
          ycenter = get.cell.meta.data("ycenter")
          pos = circlize:::polar2Cartesian(circlize(xcenter, ycenter))
          circos.text(CELL_META$xcenter, CELL_META$ycenter, 
                      meses[CELL_META$sector.numeric.index],
                      facing = "clockwise", niceFacing = TRUE,col = colotext[CELL_META$sector.numeric.index])
        })
        
        Departam_leg=stringr::str_to_title(Lis_Est$Nombre_Dep[iest])
        Municip_leg=stringr::str_to_title(Lis_Est$Municipio[iest])
        
        titulo="Calendario de lluvia (1981 - 2010)"
        title(main=titulo,cex.main = 1.5)
        legend("topleft", fill = c(coloRGB[2],coloRGB[1],coloRGB[3]),legend = c("Transición o menos Lluvioso","LLuvioso","Seco"),bty = "n",cex = 1,horiz = F)
        legend("bottomleft", legend = paste("Estación: ",codi,"\n",Municip_leg,", ",Departam_leg,sep = ""), bty = "n")
        legend("bottomright",legend="FEDEARROZ - FNA\nFuente: IDEAM",bty = "n")
        
      } 
      
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
