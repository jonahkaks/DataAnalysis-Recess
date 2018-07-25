#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinythemes)
library(shinydashboard)

# Define UI for application that draws a histogram
ui<-dashboardPage(skin = "purple",
                  dashboardHeader(title="NBA STATISTICS"),
                  dashboardSidebar(
                    sidebarMenu(
                      menuItem("DashBoard", tabName = "dashboard", icon = icon("dashboard")),
                      menuItem("Seasons", tabName = "seasons", icon = icon("th")),
                      menuItem("Statistics Summary", tabName = "seasonstat", icon = icon("pie-chart")),
                      menuItem("Players", icon = icon("user"), tabName = "players"),
                      menuItem("Player Stats", tabName = "stats", icon = icon("bar-chart"),
                               menuSubItem("Basic", tabName = "player_basic_stats"),
                               menuSubItem("Shooting", tabName = "player_shooting_stats"),
                               menuSubItem("Efficiency", tabName = "player_efficency_stats"),
                               menuSubItem("Overall Statistics", tabName = "povs")),
                      menuItem("Statistics Graph Summary", tabName = "summary", icon = icon("line-chart")),
                      menuItem("Predictions", tabName = "predictions", icon = icon("gears")),
                      menuItem("Help", tabName = "help", icon = icon("home"))
                      
                    )
                  ),
                  dashboardBody(
                    tabItems(
                      tabItem(tabName = "dashboard", 
                              fluidRow(
                                valueBoxOutput("value1")
                                ,valueBoxOutput("value2")
                                ,valueBoxOutput("value3")
                                ,valueBoxOutput("value4")
                                ,valueBoxOutput("value5")
                                ,valueBoxOutput("value6")
                              )
                      ),
                      tabItem(tabName = "seasons", h2("Seasons"), DT::dataTableOutput('seasonstat')),
                      tabItem(tabName = "seasonstat", h2("Season statistics"), verbatimTextOutput("sstats")),
                      tabItem(tabName = "players", h2("players"), DT::dataTableOutput('players')),
                      tabItem(tabName = "summary", h2("Summary of data."),
                              selectInput('element_id', label = 'Select option', choices = list("Season","PlayerData","Players"), selected = 2), 
                              
                              fluidRow(box(plotOutput("a")),
                                       box(plotOutput("b")),
                                       box(plotOutput("d")),
                                       box(plotOutput("e")),
                                       box(plotOutput("f")),
                                       box(plotOutput("g"))
                              )),
                      tabItem(tabName = "predictions", h2("Future trends as predicted by our Analysis"), fluidRow(plotOutput("currents"),
                                                                                                                  plotOutput("predictions"), 
                                                                                                                  DT::dataTableOutput('current'),
                                                                                                                  DT::dataTableOutput('predicted'))),
                      tabItem(tabName = "player_basic_stats", h2("Players Basic Statistics"),
                              selectInput('pbs', label = 'Select option',
                                          choices = c("Points"="PTS","Assists"="AST","Offensive Rebounds"="ORB",
                                                      "Defensive Rebounds"="DRB","Total Rebounds"="TRB",
                                                      "Steals"="STL","Turnovers"="TOV","Blocks"="BLK","Personal Fouls"="PF",
                                                      "Games Played"="G","Minutes Played"="MP"
                                          ), selected = 1), 
                              
                              fluidRow(
                                DT::dataTableOutput('playerbstat'), box(plotOutput("playergstat")), box(plotOutput("playertmstat"))
                              )),
                      tabItem(tabName = "player_shooting_stats", h2("Players Shooting Statistics"),
                              selectInput('pss', label = 'Select option',
                                          choices = list("Field Goal Percentage"="FG_perc","Field Goals Made"="FG","Field Goals Attempted"="FGA","Three Point Field Goal Percentage"="ThreeP_perc",
                                                         "Three Point Field Goals Made"="ThreeP","Three Point Field Goals Attempted"="ThreePA","Two Point Field Goal Percentage"="TwoP_perc",
                                                         "Two Point Field Goals Made"="TwoP","Two Point Field Goals Attempted"="TwoPA","Free Throw Percentage"="TwoP_perc","Free Throws Made"="FT",
                                                         "Free Throws Attempted"="FTA"), selected = 1), 
                              fluidRow(
                                DT::dataTableOutput('playersstat'), box(plotOutput("player_shot_stat")), box(plotOutput("player_shot_tmstat"))
                              )),
                      
                      tabItem(tabName = "player_efficency_stats", h2("Players Efficiency Statistics"),
                              selectInput('pes', label = 'Select option',
                                          choices = c(
                                            "Win Score"="WS","Game Score"="GS",
                                            "Offensive Win Scores"="OWS",
                                            "Defensive Win Scores"="DWS",
                                            "Free Throw Attempts / Field Goal Attempts"="FTA/FGA",
                                            "NBA Efficiency"="PER"
                                          ), selected = 1), 
                              fluidRow(
                                DT::dataTableOutput('playerestat'), box(plotOutput("player_eff_stat")), box(plotOutput("player_eff_tmstat"))
                              )), 
                      tabItem(tabName = "povs", h2("OverAll Player Statistics"),fluidRow(DT::dataTableOutput('mvp'))),
                      tabItem(tabName = "help", h2("Help Center"),fluidRow(
                        tags$img(src="basketballpositions.png", height="628", alt="Basketball Positions", width="675"),
                        includeHTML('help.html')
                      ))))
)

