#Running sampling function
  #Sample gain/loss
  #Sample yds using exponential distribution on both sides

#Passing sampling function
  #Sample incomplete/complete
  #Sample gain/loss
  #Sample yds

#Subset data to only include UVA

#Read in the full data
full_data <- read.csv("2019 PFF All Plays.csv")

#Separate run/pass plays
run_plays <- full_data[which(full_data$pff_RUNPASS == "R"),]
pass_plays <- full_data[which(full_data$pff_RUNPASS == "P"),]

### RUN PLAYS ###
#Get data frame showing if play was gain/loss, yardage on play
run_yds <- run_plays$pff_GAINLOSS #Yards gained/loss on play
run_yds <- na.omit(run_yds)
loss_gain <- ifelse(run_yds > 0, 1, 0) #1 if gain, 0 if loss
run_df <- as.data.frame(cbind(run_yds, loss_gain))

#Separate into data frames for loss/gain of yardage
loss_df <- abs(run_df[which(run_df$loss_gain == 0),"run_yds"]) #Absolute value necessary for exponential distribution
loss_pct <- length(loss_df)/nrow(run_df)

gain_df <- run_df[which(run_df$loss_gain == 1),"run_yds"]
gain_pct <- length(gain_df)/nrow(run_df)

#Function simulating outcome of a particular run play

run_play_sim <- function() {
  #First, sample loss or gain: use binomial distribution where p(success) = gain_pct
  run_lossgain = rbinom(1, 1, gain_pct)
  #Now, sample yards given loss/gain using exponential distribution with lambda = 1/mean(yards)
  if (run_lossgain == 0) {
    y=(-1 * round(rexp(1, rate = 1/mean(loss_df)), digits = 2)) #Multiply by -1 to show loss of yardage
  }
  else {
    y=(round(rexp(1, rate = 1/mean(gain_df)), digits = 2))
  }
  print(y)
}
run_play_sim()

### PASS PLAYS ###
#Get incompletion data to include with pass plays
incompletion <- as.character(pass_plays$pff_INCOMPLETIONTYPE)
incompletion <- ifelse(incompletion == "", "0","1") #0 if completion, 1 if incompletion
incompletion <- as.numeric(incompletion)
incompletion_pct <- length(incompletion[incompletion == 1]) / length(incompletion)

#Create data frame for all pass plays with columns showing yards and if there was an incompletion on the play
pass_yds <- pass_plays$pff_GAINLOSS
pass_df <- as.data.frame(cbind(pass_yds, incompletion))

#Create data frame for all COMPLETED pass plays with columns showing yards, if loss/gain on play
completed_passes <- pass_df[pass_df$incompletion == 0, "pass_yds"]
completed_passes <- na.omit(completed_passes)
loss_gain_pass <- ifelse(completed_passes > 0, 1, 0) #1 if gain, 0 if loss
gain_pct_pass <- length(loss_gain_pass[loss_gain_pass == 1]) / length(loss_gain_pass)
completed_passes <- as.data.frame(cbind(completed_passes, loss_gain_pass))

#Separate into data frames for loss/gain of yardage
completed_passes_loss <- abs(completed_passes[which(completed_passes$loss_gain_pass == 0),"completed_passes"]) #absolute value necessary for exponential distribution
completed_passes_gain <- completed_passes[which(completed_passes$loss_gain_pass == 1),"completed_passes"]

#Function simulating outcome of particular pass play
pass_play_sim <- function(){
incomplete = rbinom(1, 1, incompletion_pct)
  if(incomplete == 0) {
    pass_lossgain = rbinom(1, 1, gain_pct_pass)
    if (pass_lossgain == 0){ #if pass is a loss
      y = (-1 * round(rexp(1, rate = 1/mean(completed_passes_loss)), digits = 2)) #Multiply by -1 to show loss of yardage
    }
    else {
      y = (round(rexp(1, rate = 1/mean(completed_passes_gain)), digits = 2))
    }
  }
  else if(incomplete==1){
    y=0
  }
}
pass_play_sim()


#Function simulating outcome of particular pass play

run_pct<- nrow(run_plays)/(nrow(run_plays)+nrow(pass_plays))

runorpass<- function(){
   runplay = rbinom(1, 1, run_pct)
  if(runplay == 0) {
    y <- pass_play_sim()
  } else if(runplay == 1) {
    y <- run_play_sim() 
  }
}
runorpass()



#bringing it together to simulate a drive by a team
S1 <- list(A=10,B=1,C=80) #starting 
full_drive <- function(S1){
  is_not_done <- TRUE
  k <- 0
  drive_result <- list(score = NA, end_yard = NA)
  
  while(is_not_done== TRUE){
    y <- runorpass()
    C_new <- S1$C - y
    A_new <- S1$A - y
    if(A_new <=0){
      B_new <- 1
      A_new <- 10
    }else if( A_new > 0){
      B_new <- S1$B + 1
    }
    if (C_new <= 0 ){
      is_not_done <- FALSE
      drive_result$score <- 7
      print(paste0(" !Touchdown! "))
    }else if (B_new == 5){
      is_not_done <- FALSE
      drive_result$end_yard <- C_new
      print(paste0("sorry you have lost possession of the ball on downs. Your opponent gets the ball on the" , C_new, "yard line"))
    }else {
      k <- k+1
      S1$A <- A_new
      S1$B <- B_new
      S1$C <- C_new
      print(paste0(" YTG ", S1$A, " Down ", S1$B, " LOS ", S1$C, " Yards Gained ", y))
    }
    if(k > 100){
      is_not_done <- FALSE
    }
  }
  drive_result
}

full_drive(S1)


#make this whole thing into a function and then make it a loop that runs plays until someone scores
run_to_score <- function(S1)
{
  team<- 1
  k<-0
  while(k<101){
    result_drive <-full_drive(S1)
    if(!is.na(result_drive$score))
    {
      print(paste0("team", team, "scored!"))
      k<-10000
    }else{
      S<- list(A=10, B=1, C=1- result_drive$end_drive)
      team <- (team+1) %% 2
      k<- k+1
    }
  }
}
run_to_score(S1)


