## Game: Rock, Scissors, Paper

pao_ying_chup <- function() {
  ## initialize variables
  actions <- c("Rock", "Scissors", "Paper", "Exit")
  win <- 0
  loss <- 0
  tie <- 0
  
  ## start game
  cat("Rock[1], Scissors[2], Paper[3], Exit[4]\n")
  
  ## start playing games
  while (TRUE) {
    
    ## get input from user
    user_move <- as.numeric(readline("Choose your move: "))
    user_action <- actions[user_move]
    
    ## exit from the game
    if (user_action == "Exit") {
      cat("Good Bye")
      break
    }
    
    ## compare user vs. computer random
    random <- sample(1:3, size=1)
    computer_action <- actions[random]
    
    ## print actions (user vs. computer)
    cat("User:", user_action, "\n")
    cat("Computer:", computer_action, "\n")
    
    if (user_action == computer_action) {
      tie <- tie + 1
    } else if (user_action == "Rock" & computer_action == "Scissors") {
      win <- win + 1
    } else if (user_action == "Scissors" & computer_action == "Paper") {
      win <- win + 1
    } else if (user_action == "Paper" & computer_action == "Rock") {
      win <- win + 1
    } else {
      loss <- loss + 1
    }
  }
  
  print(paste("Win:", win, "Loss:", loss, "Tie:", tie))
}
