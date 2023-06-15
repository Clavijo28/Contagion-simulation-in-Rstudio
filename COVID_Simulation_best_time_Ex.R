#Below is a vector-based code to recreate the simulation of COVID19 done in python. 
#The first simulation focuses on determining the number of people and number of days, 
#in addition to this there is a third status [status = 3] 
#which represents people who were sick and are now immune, 
#in addition to this the parameter is set to start for the user to decide whether or not 
#he wants to use it by placing the percentage he would like to simulate.

#The second simulation has a vaccination plan based on the fact that 1% of the population 
#is vaccinated on a daily basis (this percentage was extracted from the current vaccination
#behavior in Australia, which is around 200 thousand people a day for second doses and 
#is given due to the fact that people already have the first dose), 
#In addition to this, the simulation will stop until the number of infected is zero.

system.time({
  
  # Evaluate the number of people to meet by each citizen 
  Evaluate_sick_people <- function(x) {
    
    # Random of number people meet
    index <- sample(0:N_population,sample(0:20,1,replace = F),replace=F)
    # Random of number of position into the total of population and evaluate if the get sick or not
    infected <- ifelse(sum(ifelse(Current_sick_people[Current_sick_people %in% index]>0, runif(1,0,1)) < Infection_rate)>=1,1,0)
  
    return(infected)
  }
  
  # Evaluate the number of people to meet by each citizen immune
  Evaluate_sick_people_immune <- function(y) {
    
    # Random of number people meet
    index <- sample(0:N_population,sample(0:People_met,1,replace = F),replace=F)
    # Random of number of position into the total of population and evaluate if the get sick or not
    infected <- ifelse(sum(ifelse(Current_sick_people[Current_sick_people %in% index]>0, runif(1,0,1)) < Immunity_rate)>=1,1,0)
  
    return(infected)
  }
  
  print(cat("Simulation 1 >> [Run the code with normal parameters]",
            "\nSimulation 2 >> [Run the code with vaccination plan]","\n"))
  
  Option <- readline(prompt="Enter number of simulation: ")
  
  # Enter number of people to evaluate
  N_population <- as.integer(readline(prompt="Enter number of people: "))
  
  # Enter number of days to evaluate
  #Number_days <- as.integer(readline(prompt="Enter number of days: "))
  
  library(ggplot2)
  
  # First sick people
  First_sick <- 2
  # Period of people sick before get healthy
  Infection_period <- 10
  # Percentage of probability to get the virus
  Infection_rate <- 0.1
  # Percentage of probability to get the virus after being sick
  Immunity_rate <- 0.001
  # Percentage of probability to die
  Mortality_rate <- 0.002
  # Number of people to meet every day
  People_met <- 20
  # Percentage of vaccination of population
  Percetage_vaccination <- 0.01
  
  # Vectors per each or attributes of infection
  #Infected_p <- vector("numeric", length = Number_days)
  #Healthy_p  <- vector("numeric", length = Number_days)
  #Immune_p   <- vector("numeric", length = Number_days)
  #Dead_p     <- vector("numeric", length = Number_days)
  #days_p     <-vector("numeric",  length = Number_days)
  
  # Status of people [1 = sick, 2 = died, 3 = immune, 4 = vaccinated]
  Status <- vector("numeric", length = N_population)
  # Position of sick people into population
  index <- sample(0:N_population,First_sick,replace = F)
  # Assign sick people into population
  Status[index] <- 1
  # Number of days of sick people
  sick_day <- vector("numeric", length = N_population)
  
  # Simulation with normal parameters
  if(Option==1){
    
    # Enter number of days to evaluate
    Number_days <- as.integer(readline(prompt="Enter number of days: "))
    
    # Vectors per each or attributes of infection
    Infected_p <- vector("numeric", length = Number_days)
    Healthy_p  <- vector("numeric", length = Number_days)
    Immune_p   <- vector("numeric", length = Number_days)
    Dead_p     <- vector("numeric", length = Number_days)
    days_p     <-vector("numeric",  length = Number_days)
    
    for(i in 0:Number_days){
      print(paste("------------------day ",i,"-------------------------"))
      
      # Current sick people into population
      Current_sick_people <- (which(Status == 1))
      # Current healthy people into population
      Current_healthy_people <- (which(Status==0))
      # Current immune people into population
      Current_immune_people <- (which(Status == 3))
      
      # Call the Evaluate_sick_people function to evaluate people that get the virus
      people_meet_index1 <- sapply(Status[Current_healthy_people], Evaluate_sick_people)
      # Assign sick people into population
      Status[which(Status==0)] <- people_meet_index1
      
      # Call the Evaluate_sick_people_immune function to evaluate people that get the virus after being sick
      people_meet_index2 <- sapply(Status[Current_immune_people], Evaluate_sick_people_immune)
      # Assign sick people (after being sick) into population
      Status[which(Status==3)] <- people_meet_index2
      
      # Assign immune people into population
      Status[which(sick_day == Infection_period)] <- 3
      # After being sick during 10 days the counter start at 0 again
      sick_day[which(sick_day == Infection_period)] <- 0
      # Counter each day sick people are
      sick_day[which(Status == 1)] <- sick_day[which(Status == 1)] + 1
      
      # Probabilities of dead per sick people
      prob_of_death <- runif(N_population,min=0,max=1)
      # Evaluate probability calculated vs Mortality_rate to know if people die or not
      Death_people <- (which(prob_of_death < Mortality_rate & Status == 1))
      # Assign dead people into population
      Status[Death_people] <- 2
      
      # Calculate and print the total of people per attribute
      print(cat("Number of people Infected >> ",T_people_infected <- sum(Status==1),
                "\nNumber of people Healthy  >> ",T_people_Healthy <- sum(Status==0),
                "\nNumber of people Immune   >> ", T_people_Immune <- sum(Status==3),
                "\nNumber of people Death    >> ", T_people_Death <- sum(Status==2),"\n"))
      
      # Assign people depend on which attribute is
      Infected_p[i] <- T_people_infected
      Healthy_p[i] <- T_people_Healthy
      Immune_p[i] <- T_people_Immune
      Dead_p[i] <- T_people_Death
      days_p[i] <- i
      
    }
    
    # Data frame of summary of virus's behavior
    Summary_Infection <- data.frame(days_p, Infected_p, Healthy_p, Immune_p, Dead_p)
    
    # Time of development of virus
    time <- as.numeric(rep(1:Number_days,times=4))
    # Merge total summary per attribute
    Merge_info <- c(Infected_p,Healthy_p,Immune_p,Dead_p)
    # Merge attributes into a vector
    Atributes <- c("Infected","healthy","Immune","Dead")
    # Group the number of days per attribute
    group <- gl(4,Number_days, labels = Atributes)
    # Summary to be able to plot the virus's behavior
    Summary_to_plot <- data.frame(time, Merge_info, group)
    
    # Print summary of infection
    print(Summary_Infection)
    
    # Graph
    Plot_Infection <- ggplot(Summary_to_plot, aes(x=time, y=Merge_info, fill=group)) + geom_area()
    print(Plot_Infection)

  }
  
  # Simulation with a vaccination plan
  if(Option==2){
    
    i <- 0
    T_people_infected <- 0
    
    # Vectors per each or attributes of infection
    Infected_p <- c()
    Healthy_p  <- c()
    Immune_p   <- c()
    Dead_p     <- c()
    days_p     <- c()
    Vaccinated_p <- c()
    
    while(T_people_infected >= 0){
      
      if( i > 100 & T_people_infected ==0 ){
        
        break
        
      }else{
        
        i <- i + 1
        
        print(paste("------------------day ",i,"-------------------------"))
          
        # Current sick people into population
        Current_sick_people <- (which(Status == 1))
        # Current healthy people into population
        Current_healthy_people <- (which(Status==0))
        # Current immune people into population
        Current_immune_people <- (which(Status == 3))
          
        # Call the Evaluate_sick_people function to evaluate people that get the virus
        people_meet_index1 <- sapply(Status[Current_healthy_people], Evaluate_sick_people)
        # Assign sick people into population
        Status[which(Status==0)] <- people_meet_index1
          
        # Call the Evaluate_sick_people_immune function to evaluate people that get the virus after being sick
        people_meet_index2 <- sapply(Status[Current_immune_people], Evaluate_sick_people_immune)
        # Assign sick people (after being sick) into population
        Status[which(Status==3)] <- people_meet_index2
        
        # Assign immune people into population
        Status[which(sick_day == Infection_period)] <- 3
        # After being sick during 10 days the counter start at 0 again
        sick_day[which(sick_day == Infection_period)] <- 0
        # Counter each day sick people are
        sick_day[which(Status == 1)] <- sick_day[which(Status == 1)] + 1
          
        # Probabilities of dead per sick people
        prob_of_death <- runif(N_population,min=0,max=1)
        # Evaluate probability calculated vs Mortality_rate to know if people die or not
        Death_people <- (which(prob_of_death < Mortality_rate & Status == 1))
        # Assign dead people into population
        Status[Death_people] <- 2
        
        # Calculate the number of people vaccinate (1% multiple per population)
        People_to_get_vaccinated <- sample(0:N_population, ifelse(Percetage_vaccination*N_population < 1, 1, Percetage_vaccination*N_population), replace =  FALSE)
        # Identify who people get the vaccine
        Status[Current_healthy_people[People_to_get_vaccinated]] <- 4
          
        # Calculate and print the total of people per attribute
        print(cat("Number of people Infected >> ",T_people_infected <- sum(Status==1),
                    "\nNumber of people Healthy  >> ",T_people_Healthy <- sum(Status==0),
                    "\nNumber of people Immune   >> ", T_people_Immune <- sum(Status==3),
                    "\nNumber of people Death    >> ", T_people_Death <- sum(Status==2),
                    "\nNumber of people Vaccinated >>", T_people_vaccinated <- sum(Status==4),"\n"))
          
        # Assign people depend on which attribute is
        Infected_p <- c(Infected_p,T_people_infected)
        Healthy_p <- c(Healthy_p,T_people_Healthy)
        Immune_p <- c(Immune_p,T_people_Immune)
        Dead_p <- c(Dead_p,T_people_Death)
        Vaccinated_p <- c(Vaccinated_p,T_people_vaccinated)
        days_p <- c(days_p,i)
          
      }
      
      }
    # Data frame of summary of virus's behavior
    Summary_Infection <- data.frame(days_p, Infected_p, Healthy_p, Immune_p, Dead_p, Vaccinated_p)
    
    # Time of development of virus
    time <- as.numeric(rep(1:i,times=5))
    # Merge total summary per attribute
    Merge_info <- c(Infected_p,Healthy_p,Immune_p,Dead_p,Vaccinated_p)
    # Merge attributes into a vector
    Atributes <- c("Infected","healthy","Immune","Dead","Vaccinated")
    # Group the number of days per attribute
    group <- gl(5,i, labels = Atributes)
    # Summary to be able to plot the virus's behavior
    Summary_to_plot <- data.frame(time, Merge_info, group)
    
    # Print summary of infection
    print(Summary_Infection)
    
    # Graph
    Plot_Infection <- ggplot(Summary_to_plot, aes(x=time, y=Merge_info, fill=group)) + geom_area()
    print(Plot_Infection)
    }
    
})
