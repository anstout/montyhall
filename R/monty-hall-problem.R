#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'   Select a door.
#'
#' @description
#'   `select_door()` randomly selects a door between 1 and 3.
#'
#' @details
#'   Due to randomization, the first door selected could contain either a goat or a
#'   car.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a number between 1 and 3 that corresponds to the
#'   `create_game()` function.
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Open a goat door.
#'
#' @description
#'   `open_goat_door()` opens one of the 2 goat doors.
#'
#' @details
#'   If the contestant first chose a goat door, then there is only on door
#'   available to open. If the contestant first chose the car door, then any of
#'   the 2 goat doors can be opened.
#'
#' @param game A vector of length 3 representing the contents of the doors (either 
#'   "car" or "goat").
#' @param a.pick An integer representing the door that the contestant has selected 
#'   (a value between 1 and 3).
#'
#' @return The function returns a number between 1 and 3 that is not the car door
#'   and not the first door that was selected.
#'
#' @examples
#'   open_goat_door()
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'   Option to switch doors.
#'
#' @description
#'   `change_door()` randomly assigns if the contestant chooses to stay with the
#'   original door or to switch to the last remaining unopened door.
#'
#' @details
#'   If the contestant stays, the final pick will be the same as the first pick.
#'   If the contestant switches, the final pick will be the last remaining unopened
#'   door.
#'
#' @param stay A logical value representing if the contestant decides to stay with  
#'   the first pick (TRUE) or switch to a different door (FALSE). The default value  
#'   is TRUE.
#' @param opened.door An integer representing the opened goat door (a value between
#'   1 and 3).
#' @param a.pick An integer representing the contestant's initial door pick (a value 
#'   between 1 and 3).
#'
#' @return The function returns a number between 1 and 3 that is either the first
#'   pick or the last remaining unopened door.
#'
#' @examples
#'   change_door(opened.door, a.pick = 2)
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#'   Determine if the contestant has won.
#'
#' @description
#'   `determine_winner()` determines whether or not the contestant has won based on
#'   the final pick.
#'
#' @details
#'   If the final pick is a car, then the contestant has won. If the final pick is
#'   a goat, then the contestant has lost.
#'
#' @param final.pick An integer representing the contestant's final pick (a value 
#'   between 1 and 3).
#' @param game A vector of length 3 representing the contents of the doors (either 
#'   "car" or "goat").
#'
#' @return The function returns either WIN or LOSE based upon the final pick.
#'
#' @examples
#'   determine_winner(final.pick = 2, game)
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'   Play one game.
#'
#' @description
#'   `play_game()` simulates one entire playthrough of the game.
#'
#' @details
#'   The function will run one course of the game and return the results.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns either WIN or LOSE based upon the final pick.
#'
#' @examples
#'   play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'   Play multiple games.
#'
#' @description
#'   `play_n_games()` simulates n number of entire playthroughs of the game.
#'
#' @details
#'   The function will run n number of courses of the game and return the results
#'   of all games.
#'
#' @param n An integer representing the number of games to be played. Default value 
#'   is 100.
#'
#' @return The function returns the number of times WIN or LOSE was the result
#'   based upon the final pick in each game.
#'
#' @examples
#'   play_n_games(n=100)
#'
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
