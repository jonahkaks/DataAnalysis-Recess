#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinythemes)
library(readr)
library(sqldf)
library(dplyr)
library(DT)
library(shinydashboard)
library(ggplot2)
shinyServer(function(input, output) {
  
  seasonnames <- c('Index','Year','Player', 'Pos','Age','Team','G','GS', 'MP','PER',
                   'TS_perc','ThreePAr','FTr','ORB_perc','DRB_perc','TRB_perc','AST_perc','STL_perc',
                   'BLK_perc','TOV_perc','USG_perc','blanl','OWS','DWS','WS','WS_48','blank2',
                   'OBPM','DBPM','BPM','VORP','FG','FGA','FG_perc','ThreeP','ThreePA','ThreeP_perc','TwoP','TwoPA',
                   'TwoP_perc','eFG_perc','FT','FTA','FT_perc','ORB','DRB','TRB','AST','STL','BLK','TOV','PF','PTS')
  seasonstats <- read.csv("Seasons_Stats.csv", header =FALSE, skip =1, col.names=seasonnames)[ ,2:53]
  seasonclean<-seasonstats
  
  seasonclean[is.na(seasonclean)]<- 0
  playerCSV <- read.csv("Players.csv", header = TRUE)
  playerdata <- read.csv("player_data.csv", header = TRUE)
  no_of_players <-sqldf("SELECT count(DISTINCT(Player)) FROM playerCSV")
  no_of_teams <-sqldf("SELECT count(DISTINCT(Team)) FROM seasonclean")
  no_of_seasons <-sqldf("SELECT count(DISTINCT(Year)) FROM seasonclean")
  player_goals <-sqldf("SELECT s.Player, SUM(s.G) AS Goals, s.Team AS Team FROM seasonclean s GROUP BY s.Player ORDER BY Goals DESC")
  no_of_players_in_pos<-sqldf("SELECT Pos AS POSITION, Count(*) AS NO FROM seasonclean GROUP BY POSITION ORDER BY NO")
  average_player_age<-sqldf("SELECT AVG(COALESCE(Age, 0)) FROM (SELECT Player, Age FROM seasonclean GROUP BY Player)")
  average_player_height<-sqldf("SELECT AVG(Height) FROM (SELECT Player, Height FROM playerCSV GROUP BY Player)")
  
  output$value1 <- renderValueBox({
    valueBox(no_of_seasons
             ,'Total Number of seasons'
             ,icon = icon("credit-card",lib='glyphicon')
             ,color = "purple")  
  })
  
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC(no_of_players, format="d", big.mark=',')
      ,'Total Number of players'
      ,icon = icon("user",lib='glyphicon')
      ,color = "green")  
  })
  output$value3 <- renderValueBox({ 
    valueBox(no_of_teams
             ,'Total Number of Teams'
             ,icon = icon("users",lib='glyphicon')
             ,color = "red")  
  })
  
  output$value4 <- renderValueBox({ 
    valueBox(formatC(no_of_players/no_of_teams, format="d")
             ,'Average number of players per team'
             ,icon = icon("users",lib='glyphicon')
             ,color = "aqua")  
  })
  output$value5 <- renderValueBox({ 
    valueBox(formatC(average_player_age, format="d")
             ,'Average Age of players'
             ,icon = icon("users",lib='glyphicon')
             ,color = "purple")  
  })
  output$value6 <- renderValueBox({ 
    valueBox(formatC(average_player_height, format="d")
             ,'Average Height Of Players'
             ,icon = icon("users",lib='glyphicon')
             ,color = "green")  
  })
  
  playeroverall <-sqldf('SELECT Year, Player,Team,Pos, SUM(G) AS Games,SUM(PTS) AS PTS, SUM(TRB)  AS TRB, SUM(AST) AS AST,
                        SUM(FG_perc) AS "FG%",SUM(ThreeP_perc) AS "FG3%",SUM(FT_perc) AS "FT%",SUM(eFG_perc) AS "eFG%",SUM(PER) AS PER,SUM(WS) AS WS,
                        SUM(G+PTS+TRB+AST+FG_perc+ThreeP_perc+FT_perc+eFG_perc+PER+WS) AS Total FROM seasonclean GROUP BY Player, Year ORDER BY Year DESC,Total DESC')
  output$mvp <-DT::renderDataTable(DT::datatable(playeroverall,class="table table-bordered", filter = 'top', options = list(scrollX = TRUE)))
  players <-sqldf("SELECT Player AS PLAYER, (height || '(cm)') AS HEIGHT, (weight || '(kg)') AS WEIGHT, collage AS COLLEGE, born AS BORN, birth_city AS BIRTH_CITY, birth_state AS BIRTH_STATE FROM playerCSV")
  output$players <- DT::renderDataTable(DT::datatable(players, class="table table-bordered", filter = 'top', options = list(scrollX = TRUE)))
  output$seasonstat <- DT::renderDataTable(DT::datatable(seasonclean, class="table table-bordered", filter = 'top', options = list(scrollX = TRUE)))
  output$player_goals <- DT::renderDataTable(DT::datatable(player_goals, class="table table-bordered", filter = 'top', options = list(scrollX = TRUE)))
  
  observeEvent(input$pbs, 
               {
                 pbs_data <-sqldf(paste('SELECT Year, Player, Age, Team,Pos, SUM(',input$pbs,') AS Value FROM seasonclean GROUP BY Player, Year ORDER BY Year DESC, Value DESC', sep=""))
                 output$playerbstat<-DT::renderDataTable(DT::datatable(pbs_data, class="table table-bordered", filter = 'top', options = list(scrollX = TRUE)))
                 pgstat <-sqldf("SELECT Pos, SUM(Value) AS v FROM pbs_data GROUP BY Pos")
                 ptmstat<-sqldf("SELECT Team, SUM(Value) AS v FROM pbs_data GROUP BY Team ORDER BY v DESC")
                 output$playergstat<-renderPlot(barplot(pgstat$v,names.arg=pgstat$Pos, legend=pgstat$Pos,
                                                        col=c("blue", "red", "yellow", "green", "black", "orange")
                                                        , main = paste("A BARGRAPH OF ",input$pbs,"AGAINST PLAYER POS"),ylab = input$pbs, xlab='Position'))
                 output$playertmstat<-renderPlot(barplot(ptmstat$v,names.arg=ptmstat$Team, legend=ptmstat$Team, col=c("blue", "red", "yellow", "green", "black", "orange"),
                                                         main = paste("A BARGRAPH OF ",input$pbs,"AGAINST PLAYER TEAM"),ylab = input$pbs, xlab='Team'))
               })
  observeEvent(input$pss,
               {
                 pss_data <-sqldf(paste('SELECT Year, Player,Team,Pos,SUM(',input$pss,') AS Value FROM seasonclean GROUP BY Player, Year ORDER BY Year DESC, Value DESC', sep=""))
                 output$playersstat<-DT::renderDataTable(DT::datatable(pss_data, class="table table-bordered", filter = 'top', options = list(scrollX = TRUE)))
                 pkk <-sqldf("SELECT Pos, SUM(Value) AS v FROM pss_data GROUP BY Pos")
                 pkkk<-sqldf("SELECT Team, SUM(Value) AS v FROM pss_data GROUP BY Team ORDER BY v DESC")
                 output$player_shot_stat<-renderPlot(barplot(pkk$v,names.arg=pkk$Pos, legend=pkk$Pos,
                                                             col=c("blue", "red", "yellow", "green", "black", "orange")
                                                             , main = paste("A BARGRAPH OF ",input$pss,"AGAINST PLAYER POS"),ylab = input$pss, xlab='Position'))
                 output$player_shot_tmstat<-renderPlot(barplot(pkkk$v,names.arg=pkkk$Team, legend=pkkk$Team, col=c("blue", "red", "yellow", "green", "black", "orange"),
                                                               main = paste("A BARGRAPH OF ",input$pss,"AGAINST PLAYER TEAM"),ylab = input$pss, xlab='Team'))
               })
  observeEvent(input$pes,
               {
                 pes_data <-sqldf(paste('SELECT Year, Player, Team,Pos,SUM(',input$pes,') AS Value FROM seasonclean GROUP BY Player, Year ORDER BY Year DESC, Value DESC', sep=""))
                 output$playerestat<-DT::renderDataTable(DT::datatable(pes_data, class="table table-bordered",filter = 'top', options = list(scrollX = TRUE)))
                 pee <-sqldf("SELECT Pos, SUM(Value) AS v FROM pes_data GROUP BY Pos")
                 peee<-sqldf("SELECT Team, SUM(Value) AS v FROM pes_data GROUP BY Team ORDER BY v DESC")
                 output$player_eff_stat<-renderPlot(barplot(pee$v,names.arg=pee$Pos, legend=pee$Pos,
                                                            col=c("blue", "red", "yellow", "green", "black", "orange")
                                                            , main = paste("A BARGRAPH OF ",input$pes,"AGAINST PLAYER POS"),ylab = input$pes, xlab='Position'))
                 output$player_eff_tmstat<-renderPlot(barplot(peee$v,names.arg=peee$Team, legend=peee$Team, col=c("blue", "red", "yellow", "green", "black", "orange"),
                                                              main = paste("A BARGRAPH OF ",input$pes,"AGAINST PLAYER TEAM"),ylab = input$pes, xlab='Team'))
               })
  output$sstats <- renderPrint({
    summary(seasonstats)
  })
  pd <- sqldf('SELECT Year, Team, SUM(PTS) AS PTS, SUM(TRB)  AS TRB, SUM(AST) AS AST,
              SUM(FG_perc) AS "FG%",SUM(ThreeP_perc) AS "FG3%",SUM(FT_perc) AS "FT%",SUM(eFG_perc) AS "eFG%",SUM(PER) AS PER,SUM(WS) AS WS,
              SUM(G+PTS+TRB+AST+FG_perc+ThreeP_perc+FT_perc+eFG_perc+PER+WS) AS Total FROM seasonclean GROUP BY Team, Year ORDER BY Year DESC,Total DESC')
  output$current<-DT::renderDataTable(DT::datatable(pd, class="table table-bordered",filter = 'top', options = list(scrollX = TRUE)))
  output$currents <- renderPlot(ggplot(pd, aes(x=Year, y=Total)) + geom_line(aes(color = Team))+ggtitle("A LINE PLOT OF TEAM PERFORMANCE TREND OVER THE YEARS") 
                                   + scale_x_continuous(limits=c(1950, 2017))
                                   + scale_y_continuous(limits=c(2500, 70000)))
  
  Model <- lm(Total~PER, data = pd)
  datpred <- predict(Model, pd, se.fit = TRUE, interval = "prediction")
  dat <- cbind(pd, datpred)
  output$predictions<- renderPlot(ggplot(dat, aes(x=Year, y=fit.fit)) + geom_line(aes(color = Team))+ggtitle("PREDICTION OF TOTAL POINTS IN THE COMING YEAR BASED PLAYER EFFICIENCY RATING") 
                                 + scale_x_continuous(limits=c(1950, 2020))
                                 + scale_y_continuous(limits=c(2500, 70000)))
  output$predicted<-DT::renderDataTable(DT::datatable(dat, class="table table-bordered",filter = 'top', options = list(scrollX = TRUE)))

  
  
  observeEvent(input$element_id, {
    if(input$element_id=="Players"){
      output$a=renderPlot(hist(playerCSV$height,breaks=10,col = "blue",main = "Histogram showing the distribution of the Height of players",xlab = "Height"))
      output$b=renderPlot(hist(playerCSV$weight,breaks=10,col = "red",main = "Histogram showing the distribution of the weight of players",xlab = "Weight"))
      output$C=renderPlot(boxplot(playerCSV$height,main="A box plot of height of players",ylab="height",col="pink"))
      output$d=renderPlot(boxplot(playerCSV$weight,main="A box plot of weight of players",ylab="weight",col="blue"))
      output$e=renderPlot(plot(playerCSV$height,playerCSV$weight,main = "A scatter plot of weight against height of players",xlab = "Height",ylab = "weight"))
      output$f=renderPlot(qqnorm(playerCSV$weight,main="Normal QQ plot of weight of players",qqline(playerCSV$weight)))
      output$g=renderPlot(qqnorm(playerCSV$height,main="Normal QQ plot of Height of players"))
    }
    if(input$element_id=="PlayerData"){
      output$a=renderPlot(plot(playerdata$year_start,playerdata$year_end,main = "A scatter plot of year_start against year_end of players",xlab = "year_start",ylab = "year_end"))
    }
    if(input$element_id=="Season"){
      output$a=renderPlot(plot(seasonstats$Year,seasonstats$FG,main = "A scatter plot of year against FG of players in different seasons",xlab = "year",ylab = "FG"))
      output$b=renderPlot(boxplot(seasonstats$FG,main="A box plot of FG of players within different seasons",ylab="FG",col="pink"))
      output$d=renderPlot(plot(seasonstats$Year,seasonstats$PTS,main = "A scatter plot of year against points of players in different seasons",xlab = "year",ylab = "PTS"))
      output$e=renderPlot(plot(seasonstats$Year,seasonstats$AST,main = "A scatter plot of year against Assists of players in different seasons",xlab = "year",ylab = "AST")) 
      output$f=renderPlot(hist(seasonstats$PF,breaks=10,col = "deepskyblue",main = "Histogram showing the distribution of Personal Fouls",xlab = "PF"))
      output$g=renderPlot(plot(seasonstats$Year,seasonstats$PER, main = "A scatter plot of year against Performance Efficiency Rating of players in different seasons",xlab = "year",ylab = "PER"))
    }})
})
