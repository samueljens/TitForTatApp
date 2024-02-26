#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(fresh)

library(DT)
library(dplyr)
library(ggplot2)


# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(
    title = "Iterated Game"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        tabName = "game",
        text = "Game",
        icon = icon("table")
      ),
      menuItem(
        tabName = "tournament",
        text = "Tournament",
        icon = icon("trophy")
      ),
      menuItem(
        tabName = "about",
        text = "About",
        icon = icon("circle-info")
      ),
      menuItem(
        tabName = "source_code",
        text = "Code",
        icon = icon("code")
      )
    )
  ),
  
  
  dashboardBody(tags$head(tags$meta(name = "viewport", content = "width=1600")),
                tags$head(tags$link(rel = "tab icon", href = "favicon.ico")),
    tabItems(
      tabItem(
        tabName = "game",
        fluidPage(
          fluidRow(
            box(width = 3, style = "height:400px;", status = "primary", solidHeader = TRUE, title = "Game Settings", 
                uiOutput("Player1_Strategy"),
                uiOutput("Player2_Strategy"),
                uiOutput("Number_Rounds"),
                div(align = "center", actionButton("button", "Play Game", style = 'color: #fff; background-color: #27ae60; padding:10px; font-size:120%')),
                textOutput("text"), align = "center", style = "font-size:120%"),
            box(width = 3, style = "height:400px;", status = "primary", solidHeader = TRUE, title = "Results",
                uiOutput("Player1_Cumulative"),
                uiOutput("Player2_Cumulative"),
                uiOutput("Game_Cumulative")),
            box(width = 6, style = "height:400px;", status = "primary", solidHeader = TRUE, title = "Plot of Game",
                plotOutput("game_line_plot", height = "40vh")),
          ),
          fluidRow(
            box(width = 12, status = "primary", solidHeader = TRUE, title = "Results Table",
                DTOutput("results_table"))
          )
        )
      ),
      
      
      # Tournament tab ----
      tabItem(
        tabName = "tournament",
        fluidPage(
          fluidRow(
            box(width = 3, style = "height:500px;", status = "primary", solidHeader = TRUE, title = "Tournament Settings",
                uiOutput("Strategy"),
                uiOutput("Game_Rounds"),
                div(align = "center", actionButton("tournament_button", "Play Tournament", style = 'color: #fff; background-color: #27ae60; padding:10px; font-size:120%')),
                textOutput("tournament_text"), align = "center", style = "font-size:120%"),
            box(width = 3, style = "height:500px;", status = "primary", solidHeader = TRUE, title = "Results",
                uiOutput("Strategy_Max"),
                uiOutput("Strategy_Min")),
            box(width = 6, style = "height:500px;", status = "primary", solidHeader = TRUE, title = "Plot of Strategies",
                plotOutput("strategy_bar_plot", height = "40vh")),
          ),
          fluidRow(
            box(width = 3, status = "primary", solidHeader = TRUE, title = "Tournament Results Table",
                DTOutput("tournament_table"))
          )
        )
    ),

      
      # About Tab ----      
      tabItem(
        tabName = "about",
        fluidPage(
          fluidRow(
            box(width = 12, solidHeader = TRUE, title = "About the Game",
                mainPanel(
                  p("This interactive application models the iterated Prisoner's Dilemma games by Robert Axelrod.
                    Further information about the game may be found in his 1981 APSR article",
                    a(href = "https://www.cambridge.org/core/journals/american-political-science-review/article/abs/emergence-of-cooperation-among-egoists/EEAB3C6460F5BC63A4DE813E1B010B21", "here.", target = "_blank"),
                    "I also recommend his 1984 book titled", 
                    a(href = "https://www.hachettebookgroup.com/titles/robert-axelrod/the-evolution-of-cooperation/9780465005642/?lens=basic-books", tags$em("The Evolution of Cooperation."), target = "_blank"), 
                    "The basic premise of the Prisoner's Dilemma is that two actors, who may thought of as individuals, political parties, institutions, or nation states, must make a decision between cooperation and defection.
                    In a game in which the players only interact once and must make a simultaneous decision, there is an incentive to defect based on the structured payoffs gained by each player.
                    The structured payoffs for Axelrod's original games, and the ones implemented in this app, may be seen below. This figure may be found in his 1981 article linked above."),
                  p("The row player, Player 1, has her payoffs listed first. The column player is Player 2. 
                    From Player 1's perspective, she earns 3 if she cooperates (and if Player 2 cooperates) but 5 if she defects (and if Player 2 cooperates).
                    Player 2 has the same perspective (should Player 1 also cooperate). Thus, there is an incentive for both players to defect."),
                  tags$img(src = "Axelrod_1981_Cooperation.png", width = "500px", height = "400px"),
                  p("However, this incentive to defect is particularly pronounced in a game played just one time. Should the game be played multiple times into the future, cooperation can develop.
                    "),
                  p("MORE DETAIL TO COME"),
                  p("The source code for creating this",
                    a(href = "https://shiny.posit.co", "Shiny app", target = "_blank"),
                    "may be found in the 'Code' section on the left-hand menu.")))
          )
        )
      ),
      
        tabItem(
          tabName = "source_code",
          fluidPage(
            fluidRow(
              box(width = 12, solidHeader = TRUE, title = "Source Code",
                  mainPanel(
                    p("The source code for this Shiny app may be found",
                      a(href = "https://github.com/samueljens/TitForTatApp", 
                        "here.", target = "_blank")))
            )
          )
        )
      )
    )
  ),
  title = "Iterated Prisoner's Dilemma"
)

# Server ----
# Define server logic
server <- function(input, output) {

  
  ### Strategies ----
  ## Define the actions of players 
  C <- "C" # Cooperate
  D <- "D" # Defect
  
  
  ## Define unique strategies as functions
  
  # Cooperate on first move and then follow opponent's prior move
  TitForTat <- function(history, opponent_history){
    # First move
    if(length(history) == 0){
      return(C)
    }
    # React to the opponent's last move
    if(tail(opponent_history, 1) == D){
      return(D)
    }
    return(C)
  }
  
  
  # Cooperate on first move and then defect only after two opponent defections in a row
  TitForTwoTat <- function(history, opponent_history){
    if(length(history) == 0){
      return(C)
    }
    if(length(history) == 1){
      return(C)
    }
    if(length(history) >= 2){
      if(tail(opponent_history, 2)[1] == D & tail(opponent_history, 2)[2] == D){
        return(D)
      }
    }
    return(C)
  }
  
  
  # Defect twice after other player defects then if other player cooperates cooperate
  TwoTitForTat <- function(history, opponent_history){
    if (length(history) == 0){
      return(C)  # Cooperate on the first move
    }
    if (tail(opponent_history, 2)[1] == D & tail(opponent_history, 1) == D){
      return(D)  # Defect twice in a row if the opponent defected in the last two moves
    }
    if (tail(opponent_history, 1) == C){
      return(C)  # Cooperate if the opponent cooperated in the last move
    }
    return(D)  # Defect otherwise
  }
  
  
  # Completely random strategy that cooperates 50% of time
  Random <- function(history, opponent_history) {
    # Generate a random choice (C or D) with equal probability
    random_decision <- sample(c(C, D), 1, prob = c(0.5, 0.5), replace = TRUE)
    return(random_decision)
  }
  
  
  # Random strategy that favors cooperating 75% of time
  RandomCooperator <- function(history, opponent_history) {
    # Generate a random choice (C or D) with equal probability
    random_decision <- sample(c(C, D), 1, prob = c(0.75, 0.25), replace = TRUE)
    return(random_decision)
  }
  
  
  # Always defect
  Defector <- function(history, opponent_history){
    return(D)
  }
  
  
  # Grim trigger: cooperate until opponent defects then always defect
  GrimTrigger <- function(history, opponent_history){
    if(length(history) == 0){
      return(C)
    }
    if("D" %in% opponent_history){
      return("D")
    }
    return("C")
  }
  
  
  # Simple Identity ChecK, red75; here: https://www.lesswrong.com/posts/hamma4XgeNrsvAJv5/prisoner-s-dilemma-tournament-results
  IdentityCheck <- function(history, opponent_history){
    if (length(history) == 0){
      return(C)  # Cooperate on the first move
    }
    if(length(history) >= 1 && length(history) <= 56){
      if(tail(opponent_history, 1) == C){
        return(C)
      } else {
        return(D)
      }
    }
    if(length(history) == 57){
      return(D) 
    }
    if(length(history) == 58){
      if(all(history[0:57] == c(C, C, rep(D, 55))) && all(opponent_history[0:57] == c(C, C, rep(D, 55)))){
        return(C)
      } else {
        return(D)
      }
    }
    # Default: Cooperate
    return(C)
  }
  

  # Define function to play iterated games
  play_game <- function(player1_strategy, player2_strategy, rounds){
    
    results <- data.frame(Round = integer(), Player1 = character(), Player2 = character(), stringsAsFactors = FALSE)
    
    player1_history <- c()
    player2_history <- c()
    
    for (i in 1:rounds) {
      
      # Get the actions of both players based on their strategies
      player1_action <- player1_strategy(player1_history, player2_history)
      player2_action <- player2_strategy(player2_history, player1_history)
      
      # Update the history
      player1_history <- c(player1_history, player1_action)
      player2_history <- c(player2_history, player2_action)
      
      # Print or store the results as needed
      cat("Round:", i, "Player 1:", player1_action, "Player 2:", player2_action, "\n")
      
      results <- rbind(results, data.frame(Round = i, Player1 = player1_action, Player2 = player2_action))
    }
    
    
    results$Player1_Payoff <- ifelse(results$Player1 == C & results$Player2 == C, 3,
                                     ifelse(results$Player1 == C & results$Player2 == D, 0,
                                            ifelse(results$Player1 == D & results$Player2 == C, 5,
                                                   ifelse(results$Player1 == D & results$Player2 == D, 1, NA))))
    
    
    results <- within(results, Player1_Cumulative <- cumsum(Player1_Payoff))
    
    
    results$Player2_Payoff <- ifelse(results$Player2 == C & results$Player1 == C, 3,
                                     ifelse(results$Player2 == C & results$Player1 == D, 0,
                                            ifelse(results$Player2 == D & results$Player1 == C, 5,
                                                   ifelse(results$Player2 == D & results$Player2 == D, 1, NA))))
    
    
    results <- within(results, Player2_Cumulative <- cumsum(Player2_Payoff))
    
    
    results$Total_Payoff <- as.numeric(results$Player1_Payoff + results$Player2_Payoff)
    
    
    results <- within(results, Cumulative_Payoff <- cumsum(Total_Payoff))
    
    
    return(results)
  }
  
  #play_game2 <- function(player1_strategy, player2_strategy, rounds, payoff_matrix) {
  #  results <- data.frame(
  #    Round = 1:rounds,
  #    Player1 = character(rounds),
  #    Player2 = character(rounds),
  #    stringsAsFactors = FALSE
  #  )
    
  #  payoff_matrix <- matrix(c(3, 0, 5, 1), nrow = 2, byrow = TRUE)
    
  #  player1_history <- character(0)
  #  player2_history <- character(0)
    
  #  for (i in 1:rounds) {
  #    player1_action <- player1_strategy(player1_history, player2_history)
  #    player2_action <- player2_strategy(player2_history, player1_history)
      
  #    player1_history <- c(player1_history, player1_action)
  #    player2_history <- c(player2_history, player2_action)
      
  #    results[i, c("Player1", "Player2")] <- c(player1_action, player2_action)
  #  }
    
  #  results$Player1_Payoff <- with(
  #    results,
  #    ifelse(Player1 == "C" & Player2 == "C", payoff_matrix[1, 1],
  #           ifelse(Player1 == "C" & Player2 == "D", payoff_matrix[1, 2],
  #                  ifelse(Player1 == "D" & Player2 == "C", payoff_matrix[2, 1],
  #                         ifelse(Player1 == "D" & Player2 == "D", payoff_matrix[2, 2], NA)))
  #    )
  #  )
    
   # results$Player1_Cumulative <- cumsum(results$Player1_Payoff)
    
  #  results$Player2_Payoff <- with(
  #    results,
  #    ifelse(Player2 == "C" & Player1 == "C", payoff_matrix[1, 1],
  #           ifelse(Player2 == "C" & Player1 == "D", payoff_matrix[1, 2],
  #                  ifelse(Player2 == "D" & Player1 == "C", payoff_matrix[2, 1],
  #                         ifelse(Player2 == "D" & Player2 == "D", payoff_matrix[2, 2], NA)))
  #    )
   # )
    
  #  results$Player2_Cumulative <- cumsum(results$Player2_Payoff)
    
  #  results$Total_Payoff <- as.numeric(results$Player1_Payoff + results$Player2_Payoff)
  #  results$Cumulative_Payoff <- cumsum(results$Total_Payoff)
    
  #  return(results)
  #}
  
  
  
  decision_rules <- list(
    "Tit for Tat" = TitForTat,
    "Tit for Two Tat" = TitForTwoTat,
    "Two Tit for Tat" = TwoTitForTat,
    "Random 50/50" = Random,
    "Random 75/25" = RandomCooperator,
    "Defector" = Defector,
    "Grim Trigger" = GrimTrigger,
    "Identity Check" = IdentityCheck)
  
  
  # Game Tab -----
  # Filters
  
  output$Player1_Strategy <- renderUI({
    pickerInput("Player1_Strategy", "Select Player 1's Strategy",
                choices = names(decision_rules))
  })
  
  
  output$Player2_Strategy <- renderUI({
    pickerInput("Player2_Strategy", "Select Player 2's Strategy",
                choices = names(decision_rules))
  })
  
  
  output$Number_Rounds <- renderUI({fluidPage(
    numericInput("Number_Rounds", "Number of Rounds:", 200, min = 50, max = 2000),
    verbatimTextOutput("rounds"))
  })
  
  
  output$text <- renderText("Press the 'Play Game' button to begin.")
  

  # Reactive Data -----
  reactive_game_results <- eventReactive(input$button, {
    player1_strategy_name <- input$Player1_Strategy
    player2_strategy_name <- input$Player2_Strategy
    rounds <- input$Number_Rounds
    
    req(player1_strategy_name, player2_strategy_name, rounds)
    
    player1_strategy <- decision_rules[[player1_strategy_name]]
    player2_strategy <- decision_rules[[player2_strategy_name]]
    
    isolate(play_game(player1_strategy, player2_strategy, rounds))
    
  })
  
  
  # Results Table -----
  output$results_table <- renderDT({
    results <- reactive_game_results()
    if(!is.data.frame(results)){
      results <- as.data.frame(results)
    }
    results %>%
      select(Round,
             "Player 1" = Player1,
             "Player 2" = Player2,
             "Player 1 Payoff" = Player1_Payoff,
             "Player 2 Payoff" = Player2_Payoff,
             "Player 1 Cumulative" = Player1_Cumulative,
             "Player 2 Cumulative" = Player2_Cumulative,
             "Combined Payoff" = Total_Payoff,
             "Cumulative Payoff" = Cumulative_Payoff) %>%
    datatable(results,
              rownames = FALSE,
              escape = FALSE, # Allow HTML
              options = list(paging = TRUE,
                             scrollY = "30vh",
                             autoWidth = FALSE,
                             scrollX = TRUE,
                             pageLength = 10))
  })
  
  
  # Value Boxes -----
  # Player 1 Cumulative Payoff
    output$Player1_Cumulative <- renderValueBox({
      cumulative <- reactive_game_results()$Player1_Cumulative[nrow(reactive_game_results())]
      
    valueBox(value = format(cumulative, big.mark = ","), color = "red", subtitle = "Player 1's Cumulative Payoff")
  })
  
  
  # Player 2 Cumulative Payoff
  output$Player2_Cumulative <- renderValueBox({
    cumulative <- reactive_game_results()$Player2_Cumulative[nrow(reactive_game_results())]
    
    
    valueBox(value = format(cumulative, big.mark = ","), color = "blue", subtitle = "Player 2's Cumulative Payoff")
  })
  
  
  # Total Cumulative Payoff
  output$Game_Cumulative <- renderValueBox({
    cumulative <- reactive_game_results()$Cumulative_Payoff[nrow(reactive_game_results())]
    
    valueBox(value = format(cumulative, big.mark = ","), color = "black", subtitle = "Total Cumulative Payoff")
  })
  
  
  # Plot of game -----
  output$game_line_plot <- renderPlot({
    ggplot(data = reactive_game_results(), mapping = aes(x = Round)) + 
      geom_line(aes(y = Player1_Cumulative, color = 'Player 1'), show.legend = TRUE) + 
      geom_line(aes(y = Player2_Cumulative, color = 'Player 2'), show.legend = TRUE) +
      geom_abline(slope = 3, intercept = 0, color = 'black', lty = 3) + 
      ylim(c(1, 3 * nrow(reactive_game_results()))) +
      ylab("A Player's Cumulative Payoff") +
      theme_classic() +
      scale_color_manual(values = c("Player 1" = "red", "Player 2" = "blue"),
                         name = "Player") + 
      theme(legend.position = c(.15, .80)) #+
      #      legend.title = element_text(size = 14), 
      #      legend.text = element_text(size = 12)) 
      #theme(text = element_text(size=rel(4.5)))
  })

  
  ## Tournament Page ----
  # Strategies in tournament
  output$Strategy <- renderUI({
    checkboxGroupInput("Strategy", "Select Decision Strategies in Tournament:",
                       choices = names(decision_rules))
  })
  
  
  output$Game_Rounds <- renderUI({
    numericInput("Game_Rounds", "Number of Rounds:", 200, min = 50, max = 2000)
  })
  
  
  output$tournament_text <- renderText("Press the 'Play Tournament' button to begin. Please wait a few moments while the results are calculated.")


  # Reactive tournament results ----
  reactive_tournament_results <- eventReactive(input$tournament_button, {
    tournament_results <- data.frame(Player1 = character(), Player2 = character(), Payoff1 = numeric(), Payoff2 = numeric())
    
    # Iterate through each combination of strategies
    for (player1_strategy_name in input$Strategy) {
      for (player2_strategy_name in input$Strategy) {
        
        # Skip the same strategy combination (however, not in Axelrod's tournament)
        # if (player1_strategy_name == player2_strategy_name) { # IMPORTANT
        #   next
        # }
        
        # Run the iterated game for a certain number of rounds (you can modify this)
        player1_strategy <- decision_rules[[player1_strategy_name]]
        player2_strategy <- decision_rules[[player2_strategy_name]]
        game_results <- play_game(player1_strategy, player2_strategy, input$Game_Rounds)
        
        # Store the results in the all_results data frame
        tournament_results <- rbind(tournament_results, data.frame(Player1 = player1_strategy_name,
                                                                   Player2 = player2_strategy_name,
                                                                   Payoff1 = game_results$Player1_Cumulative[nrow(game_results)],
                                                                   Payoff2 = game_results$Player2_Cumulative[nrow(game_results)]))
      }
    }
    
    strategy1 <- tournament_results[, c(1, 3)]
    colnames(strategy1) <- c("Strategy", "Payoff")
    
    strategy2 <- tournament_results[, c(2, 4)]
    colnames(strategy2) <- c("Strategy", "Payoff")
    
    strategy_dat <- rbind(strategy1, strategy2)
    
    strategy_average <- aggregate(Payoff ~ Strategy, strategy_dat, mean)
    
    return(strategy_average)
  })
  

  output$Strategy_Max <- renderValueBox({
    max_payoff <- max(reactive_tournament_results()$Payoff)
    
    valueBox(value = format(max_payoff, big.mark = ","), color = "green", subtitle = "Maximum Payoff in Tournament")
  })
  
  
  output$Strategy_Min <- renderValueBox({
    min_payoff <- min(reactive_tournament_results()$Payoff)
    
    valueBox(value = format(min_payoff, big.mark = ","), color = "red", subtitle = "Minimum Payoff in Tournament")
  })
  
  
  # Tournament Results Table -----
  output$tournament_table <- renderDT({
    results2 <- reactive_tournament_results()
    if (!is.data.frame(results2)) {
      results2 <- as.data.frame(results2)
    }
   
    if ("Payoff" %in% colnames(results2)) { # Helpful code that is necessary to sort after running games
      results2 <- results2[order(-results2$Payoff), ]
    }
    
    
    results2 <- results2 %>%
      datatable(rownames = FALSE,
                options = list(paging = TRUE,
                               scrollY = "30vh",
                               autoWidth = FALSE,
                               scrollX = TRUE,
                               pageLength = 10,
                               lengthMenu = c(10, 15, 20)))
  })
  
  
  # Tournament Plot ----
  output$strategy_bar_plot <- renderPlot({
    ggplot(data = reactive_tournament_results(), mapping = aes(x = Strategy, y = Payoff)) + 
      geom_bar(stat = "identity", fill = "gray30") + 
      theme_classic() + 
      theme(text = element_text(size=rel(4.5))) + 
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)

