
################################################################################
##################Required and recommended packages#############################
################################################################################

if(!require(devtools)){install.packages("devtools")}; library(devtools) # To install packages from Github
if(!require(MoBPS)){devtools::install_github("tpook92/MoBPS", subdir="pkg")}; library(MoBPS) # to extract population, breeding values, cohorts, generations, time, etc data from RData
if(!require(ggplot2)){install.packages("ggplot2")}; library(ggplot2) # To plot aesthetic and representative plots (box plots, line plots)
if(!require(RandomFieldsUtils)){install.packages("RandomFieldsUtils")}; library(RandomFieldsUtils) #Recomended for MoBPS to reduce computing time
if(!require(miraculix)){install.packages("miraculix")}; library(miraculix) #required by mobps # recomended for MoBPS to reduce computing time
if(!require(plotly)){install.packages("plotly")}; library(plotly) #to add interactivity and fluidity to ggplots
if(!require(htmltools)){install.packages("htmltools")}; library(htmltools)#to create html tags and document
if(!require(htmlTable)){install.packages("htmlTable")}; library(htmlTable)#to do html table


#Function to extract population rdata from Mobps run and plot TBV, Average BV, Inbreeding
#development With options to get data and get HTML summary

do_plot<-function(){
  #Introducing and giving information of what is required by the function
  cat("************************* The program has started *************************","\n","\n")
  cat("This program will extract BV and inbreeding data obtained from runs of Modular\nBreeding Program Simulator (MOBPS).It allows its users to plot True Breeding\nValue box plot, Average Breeding Value line plots, development of inbreeding\nacross time and generations line plots, options to save the data, and options to\nget HTML summary.\n\n")
  
  cat("Loading of the following packages is necessary for the program to\nwork\n#1. MoBPS package: To extract, population, breeding value, cohorts,\n    generation, time, traits etc from RData\n#2. ggplots2 package: To plot aesthetic and representative plots (\n    box plots, line plots)\n#3. plotly package: to add interactivity and fluidity to ggplots\n#4. RandomFieldUtils package: Recomended for MoBPs to reduce computing time\n#5. miraculix package: Recomended for MoBPs to reduce computing\n    time\n#6. devtools package Needed to instal packages from Github\n#7. htmltools package: To create html tags and document\n#8. htmlTable package: to create html tables","\n","\n")
  
  cat("The program need MoBPS derived RData as input. If you\nhave such RData file, please enter without quotation mark the\ncomplete file path.\nFor example: c:/Users/Myname/Documents/Mobps_derived.RData\nOr if you are working at a working directory type just the file name\nFor example: Mobps_derived.RData\n","\n")
  
  
  
  
  
  ##############################################################################
  #################BASIC DATA INITIALIZATION STEP###############################
  ##############################################################################
  
  #The following while loop make sure the user enters the correct path and file 
  #type. It make sure the program exit if the user don't have the Rdata or if the
  #user just want to exit, 
  #The central thing it does other than checking user input is Loading the user 
  #supplied RData obtained from a Mobps run and saving it to the variable pop
  
  while(TRUE){
    
    tryCatch( #Adding a layer of error catching in case user provided unacceptable file type
      {
        
        cat("Type: '1' to Enter rdata path\nType: 'Any-Key' to exit the program",
            "\n")
        con <- readline("---------->:") #Here the user choice received
        
        if (con == 1){
          rdata<-readline(prompt="Please type MoBPs derived Rdata here without quotation mark: ")
          if (file.exists(rdata)){
            pop <- load(rdata) #Here it loads the rdata if the file exist
            break
          }else if (!file.exists(rdata)){
            cat("Wrong file or path!!! Please try again.","\n")
          }
        } else {return("Bye")}
        
        
        #Extracting the object and saving it to the variable population
        population <- get(pop)
        
      }, error=function(e){
        message(cat("Try Again!!!\n"))   
        message(cat("The file or its path caused an Error","\n"))
        message(cat("The program is spared from a crush caused by:\n"))
        message(e)
        message(cat("\nTry again by providing RData file obtained from MoBPS\n"))
      },
      warning = function(e){
        message(cat("Try Again!!!\n"))   
        message(cat("The file or its path raised a Warning","\n"))
        message(cat("The program is spared from a crush caused by:\n"))
        message(e)
        message(cat("\nTry again by providing RData file obtained from MoBPS\n"))
      }
    )
    
  } 
  #Using Mobps package get.cohorts extracting cohorts and saving them in
  #the variable cohorts
  cohorts <- data.frame(get.cohorts(population, extended = T))
  
  #Extracting unique cohorts names and saving them in the variable coht
  coht <- unique(cohorts$name)
  
  #using Mobps package population$info$trait.name extracting the traits
  #the number of traits could be one or more than one
  trait <- population$info$trait.name
  
  #The varaiable t saves the length or the number of traits. This variable will
  #be used to refer the number of traits.
  t <- length(trait) #unused t
  
  #Initialization of empty vectors TBV, Cohorts, TimeP for data frame
  #construction
  TBV = c()
  Cohorts <- c()
  TimeP <- c()
  
  #Loop to extract TBV, Cohorts and TimeP and saving them to their respective
  #empty vectors
  for (i in 1:length(coht)) {
    bv <- get.bv(population, cohorts = coht[i])
    timeP <- get.time.point(population, cohorts = coht[i])
    TBV <- append(TBV, bv)
    Cohorts <- append(Cohorts, rep(coht[i], length(bv)))
    TimeP <- append(TimeP, timeP)
  }
  
  #Forming of data frame nemed Fdat by joining the vectors TBV, Cohorts, TimeP
  #The data frame Fdat saves the data without separating the traits
  Fdat <- data.frame(TBV, Cohorts, TimeP, row.names = NULL)
  
  #The following three lines will create the Traits new columns on the Fdat
  Traits = c()
  Traits <- rep(trait, nrow(Fdat) / length(trait))
  Fdat$Traits <- Traits
  
  #Extracting the cohorts with repeats from mobps naming of repeats grabbing "_"
  #and saving to Cohort_Rept
  Cohort_Rept <- Fdat$Cohorts[grep("_", Fdat$Cohorts)]
  
  
  #The following loop will Extract what is common to the repeated Cohorts name or
  #from Cohort_Rept and save to vector Cohort_Rept_Com
  Cohort_Rept_Com <- c()
  for (i in 1:length(Cohort_Rept)) {
    Cohort_Rept_Com[i] <- sub("\\_.*", "", Cohort_Rept[i])
  }
  
  
  #The following loop will grab or extract what is unique that is the number
  #after "_" as unique identifier between repeats of same class
  Cohort_Rept_ID <- c()
  for (i in 1:length(Cohort_Rept)) {
    Cohort_Rept_ID[i] <-
      as.integer(sub("^([^_]+_){1}([^_]+).*", "\\2", Cohort_Rept[i]))
  }
  
  
  #Creating data frame that contain the above three vectors to save the unique
  #naming and id of the repeats
  Repeats_DF <- data.frame(Cohort_Rept, Cohort_Rept_ID, Cohort_Rept_Com)
  
  
  #The following will subset Fdat extracting all the columns and rows that have
  #cohorts repeats and save it to temporary data frame or TempRepDat
  TempRepDat <- subset(Fdat, is.element(Fdat$Cohorts, Cohort_Rept))
  
  
  #The following will bind the columns of TempRepDat (Which contain repeats
  #cohorts TBV among other three columns) and Repeats_DF (which contain the uniqu
  #cohort naming and Id of the repeats) then create new dataframe combining both
  #RepDat
  RepDat <- cbind(TempRepDat, Repeats_DF)
  
  
  #The following will subset Fdat extracting all the columns and rows that not
  #have cohorts repeats and save it to NonRepDat dataframe
  NonRepDat <- subset(Fdat,!is.element(Fdat$Cohorts, Cohort_Rept))
  
  
  #The following three lines add three new columns to facilitate for the next
  #rbind operation
  NonRepDat$Cohort_Rept <- NonRepDat$Cohorts
  NonRepDat$Cohort_Rept_ID <- rep(0, nrow(NonRepDat))
  NonRepDat$Cohort_Rept_Com <- NonRepDat$Cohorts
  
  
  #The following will rbind the None repeats and the Repeats TBV data in to one
  #master data called Fdat
  Fdat <- rbind(NonRepDat, RepDat)
  
  
  
  ##############################################################################
  #################END OF BASIC DATA INITIALIZATION STEP########################
  ##############################################################################
  
  #   #   #   # #   #   #   # #   #   #   # #   #   #   # #   #   #   # #   #  #
  
  #############################################################################
  ###################TRAITS SEPARATION of data saved in Fdat###################
  #############################################################################
  
  #Initializing empty list() to save the separated traits value of TBV, Cohorts,
  #and TimeP
  listTraits <- list()
  
  
  #The for loop to initialize based on the number of traits
  #empty data frames for each traits with columns TBV, Cohorts and TimeP
  for (i in 1:length(trait)) {
    assign(
      trait[i],
      data.frame(TBV = c(0), Cohorts = c(0), TimeP = c(0), Traits = c(0),
                 Cohort_Rept = c(0), Cohort_Rept_ID = c(0), Cohort_Rept_Com = c(0)
      )
    )
    lst <- paste(trait[i], sep = "")
    ls <- get(paste(trait[i], sep = ""))
    listTraits[[lst]] <- ls
    
  }
  
  #The for loop to fill up the created empty data frames with in the listTraits
  #list with separated rows of each respective traits from the initial data frame
  #Fdat row values of TBV, Cohorts, TimeP to their respective data frame within the
  #listTraits
  
  for (i in 1:length(trait)) {
    listTraits[[i]] <- subset(Fdat, Fdat$Traits == trait[i])
  }
  
  
  ##############################################################################
  #####################END OF TRAITS SEPARATION#################################
  ##############################################################################
  
  #   #   #   # #   #   #   # #   #   #   # #   #   #   # #   #   #   # #   #  #
  
  ##############################################################################
  ##CALCULATING AVERAGE BREEDING VALUE of cohorts with repeats FOR EACH TRAITS##
  ##############################################################################
  
  #Initializing empty list AverageBVList to save data frames of Average BV,
  #Cohorts and TimeP for each traits
  AverageBVList <- list()
  
  #For loopInitializing empty data frames within the list AverageBVList to save
  #in the future the separage AverageBV, Cohorts, TimeP based on each traits
  for (i in 1:length(trait)) {
    assign(
      trait[i],
      data.frame(
        AverageBV = c(), Cohorts = c(), TimeP = c(), Traits = c(), 
        Cohort_Rept = c(), Cohort_Rept_ID = c(), Cohort_Rept_Com = c()
      )
    )
    Avglst <- paste(trait[i], sep = "")
    Avls <- get(paste(trait[i], sep = ""))
    AverageBVList[[Avglst]] <- Avls
  }
  
  #loop to calculate average of BV for cohorts and saving them for each trait in AverageBVList list
  for (i in 1:length(trait)) {
    AverageBVList[[i]] <-
      aggregate(
        TBV ~ Cohorts+TimeP+Traits+Cohort_Rept+Cohort_Rept_ID+Cohort_Rept_Com,
        listTraits[[i]],
        mean
      )
    AverageBVList[[i]] <- AverageBVList[[i]][, c(7, 1, 2, 3, 4, 5, 6)]
    AverageBVList[[i]] <-
      rename(
        AverageBVList[[i]],
        c(
          "AverageBV" = "TBV",
          "Cohorts" = "Cohorts",
          "TimeP" = "TimeP",
          "Traits" = "Traits",
          "Cohort_Rept" = "Cohort_Rept",
          "Cohort_Rept_ID" = "Cohort_Rept_ID",
          "Cohort_Rept_Com" = "Cohort_Rept_Com"
        )
      )
    
  }
  
  # This Step will create a uniqueCohortsDF which contain all the cohorts 
  # classes which are common for the cohort repeats.
  
  UniqueCohorts <- c(unique(Cohorts))
  CohortsIndex <- c(1:length(UniqueCohorts))
  
  uniqueRepeats <- c(unique(Fdat$Cohort_Rept_Com))
  uniqueRepeatsIndex <- c(1:length(uniqueRepeats))
  
  uniqueRepeatsDF <- data.frame(uniqueRepeatsIndex, uniqueRepeats)
  uniqueCohortsDF <- data.frame(CohortsIndex, UniqueCohorts)
  
  ##############################################################################
  #########END OF CALCULATING AVERAGE BREEDING VALUE FOR EACH TRAITS############
  #########       ,Cohorts, and their repeats.                      ############
  ##############################################################################
  
  #   #   #   # #   #   #   # #   #   #   # #   #   #   # #   #   #   # #   #  #
  
  ##############################################################################
  #########Dialog based USER INTERFACE FOR PLOTTING BV, DEVELOPMENT OF##########
  #########   INBREEDING, GETTING DATA AND GETTING HTML               ##########
  ##############################################################################
  
  #This is just giving a brief information that the program started and 
  #a small introduction for the user what he can and should do
  cat("*********************** RData extracted successfuly ***********************","\n","\n")
  cat("**************************************************************************","\n")
  cat("***************************** Introduction ******************************","\n")
  cat("**************************************************************************", "\n")
  cat("This program will extract BV and inbreeding data obtained from runs of Modular\nBreeding Program Simulator (MOBPS).It allows its users to plot True Breeding\nValue box plot, Average Breeding Value line plots, development of inbreeding\nacross time and generations, options to save the data, and options to get HTML\nsummary.\n\nThe plots will be desplayed depending on where the function is called.\n\n****>If the user calls the function from the console and R Script:\n******>The plots will be desplayed under Viewer.\n********>In this case the user has options to:\n**********>Navigate between plots using '<-Go back' and '->Go forward',\n**********>Zoom in and Zoom out plots,\n**********>Export options such as to save plots as image and html and\n**********>Open the plots in a new window.\n\n****>If the user call the function from R Markdown and R Notebook,\n******>Plots will be displayed below the code chunck after exiting the function.\n********>In this case the user has options to:\n**********>Open the plots in a new window with expanded view.\n\n****>Regardless of where the user calls the function\n******>Html summary is displayed under Viewer.\n********>In this case the user has options to:\n**********>Navigate between HTML and plots using '<-Go back' and '->Go forward',\n**********>Zoom in and Zoom out of HTML,\n**********>Export options such as to save HTML as image and html (for example to share it) and\n**********>Open the plots in a new window or web browser.\n\nThe program will guide its users. The user should normally type the given\noption numbers to get the desired output.","\n","\n")  cat("************************* Please follow the guide **************************","\n")
  
  
  #Following is the outer while loop which guide and insist the 
  #user to either continue using the program or exit peacefully without crushing
  while (TRUE) {
    cat("Type: 1 to continue using the program\nType: 2 to exit the program",
        "\n")
    continue <- readline("---------->:") #Here the user choice received
    
    #The following conditional is a door to the 4 main menu of the program.
    if (continue == 1) {
      
      
      
      #The following is embeded while loop making sure the user type 
      #between 1 to 4 only
      
      while (TRUE) {
        #Here the 4 choises presented to the user
        cat(
          "Type: 1 for Breeding Value Plots\nType: 2 for Inbreeding Development Plots\nType: 3 for Getting Data\nType: 4 for Getting Html Summary", "\n")
        choice <- readline("--------------->:") #Here the user choise received: 1/2/3/4
        #The following conditional is just assigning the work based on user choice
        if (choice == 1 || choice == 2 || choice == 3 || choice == 4) {
          #The following conditional is where the TBV and Avg.BV plots done, and
          #within it there are also choices for the user to choose
          if (as.numeric(choice) == 1) {
            #The following is another embeded while loop to control the user input
            while (TRUE) {
              #Here the user introduced to choose two options
              cat(
                "Type: 1 for ploting TBV\nType: 2 for plotting Average Breeding Value based on repeats",
                "\n"
              )
              BVoptions <- readline("-------------------->:") #Here the user choice received or saved
              #The following conditional processes the user choise and do one of the two tasks
              if (BVoptions == 1 || BVoptions == 2) {
                #The following conditional is where the TBV box plots done, and within it there are 
                #further choises for the user
                if (BVoptions == 1) {
                  #The following is another embedded while loop to control the user input
                  while (TRUE) {
                    #Below the user introduced to choose one of the three choices
                    cat("Type: 1 for ploting TBV of all cohorts\nType: 2 for ploting TBV of selected cohorts\nType: 3 for ploting TBV of selected cohorts repeats","\n")
                    TBVplotOptions <- readline("------------------------->:") #Here the user choice received
                    #The following conditional is just assigning the work based on user choice
                    if (TBVplotOptions == 1 || TBVplotOptions == 2 || TBVplotOptions == 3) {
                      #The following conditional is where box plot of TBV for all cohorts done
                      if (TBVplotOptions == 1) {
                        
                        ggplotlist <- list()#initializing list to save the box plots of different traits
                        
                        #The following for loop will do ggplot/ggplotly box plots and save them to the empty list for each available traits 
                        for (i in 1:length(trait)) {
                          #dev.new()
                          listTraits2e <-listTraits[[i]][order(listTraits[[i]]$TimeP),]
                          listTraits[[i]]$Cohorts <-factor(listTraits[[i]]$Cohorts,
                                                           levels = unique(listTraits2e$Cohorts))
                          #Here is the actual plot, where eval evaluate in the current environment that it
                          #gets from substitute unevaluated ggplot from listTraits[1:length(trait)] it will deal 
                          #with r for loop issues http://adv-r.had.co.nz/Computing-on-the-language.html, 
                          #https://stackoverflow.com/questions/31993704/storing-ggplot-objects-in-a-list-from-within-loop-in-r
                          f1 <- eval(substitute(ggplotly(ggplot(
                            data = listTraits[[i]], mapping = aes(
                              x = interaction(Cohorts, TimeP,lex.order = TRUE,sep = ", T="),
                              y = TBV, fill=Cohorts)) +
                              theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
                                    legend.position = "topright", legend.text = element_text(size = 6.5)) +
                              xlab("Cohorts, Time") +
                              ylab("True Breeding Value") +
                              geom_boxplot(width = 0.25,colour = "#3366FF", notch = T) +
                              theme(plot.background = element_rect(fill ="lightcyan", colour = "orange"))+
                              ggtitle(paste("TBV ", trait[i]))),
                            list (i = i)
                          ))
                          ggplotlist[[i]] <- f1 #putting in the list the plots
                          
                        }
                        #displaying or printing the plots one by one
                        for (i in 1:length(trait)) {
                          print(ggplotlist[[i]])
                        }
                        
                      }
                      #conditional to plot TBV box plots of a selected cohort by the user
                      else if (TBVplotOptions == 2) {
                        
                        View(uniqueCohortsDF) #Here it will show the user the different unique cohorts and their index
                        
                        cat("Select cohort by the given index:\n--->To select type the index number and press Enter\n--->If you are done selecting press Enter twice")
                        
                        ##
                        y1<-uniqueCohortsDF$CohortsIndex
                        #Here the user selection of cohorts received while checking for error input
                        while (TRUE){
                          selectedCohortIndex<-scan()
                          selectedCohortIndex<-unique(selectedCohortIndex)
                          z1<-length(selectedCohortIndex)
                          u1<-sum(is.element(selectedCohortIndex, y1))
                          if(u1==z1){break}
                          else{
                            cat ("One or more index you typed is not correct.\nPlease try again by typing only the given index","\n")
                          }
                          
                        }
                        
                        
                        
                        
                        ##
                        
                        
                        vectorSelected <- c(uniqueCohortsDF$UniqueCohorts[selectedCohortIndex]) #put the user selected indexes to a vector
                        #The following is empty list initialization 
                        LstempDat <- list()
                        #The following for loop creates empty data frame according to the number of traits
                        for (i in 1:length(trait)) {
                          assign(
                            trait[i],
                            data.frame(
                              TBV = c(),
                              Cohorts = c(),
                              TimeP = c(),
                              Traits = c(),
                              Cohort_Rept = c(),
                              Cohort_Rept_ID = c(),
                              Cohort_Rept_Com = c()
                            )
                          )
                          #the following 3 lines paste the traits to the empty list data frames
                          Selectedlst <- paste(trait[i], sep = "")
                          Selectedls <-get(paste(trait[i], sep = ""))
                          LstempDat[[Selectedlst]] <- Selectedls
                          
                        }
                        # the following for loops is taking care of users selection of index
                        # filtering out the cohorts 
                        for (k in 1:length(trait)) {
                          
                          for (i in 1:length(vectorSelected)) {
                            temp <-
                              subset(listTraits[[k]],
                                     vectorSelected[i] == listTraits[[k]]$Cohorts)
                            LstempDat[[k]] <-
                              rbind(LstempDat[[k]], temp)
                          }
                        }
                        
                        
                        
                        for (i in 1:length(trait)) {
                          LstempDat[[i]]$Cohorts <-
                            with(LstempDat[[i]],
                                 factor(Cohorts, levels = unique(Cohorts)))
                        }
                        
                        ggplotsSelTBV <- list() #initializing an empty list
                        #for loop to do the actual plot
                        for (i in 1:length(trait)) {
                          #dev.new()
                          f2 <-
                            eval(substitute(
                              ggplotly(
                                ggplot(
                                  data = LstempDat[[i]],
                                  mapping = aes(
                                    x = interaction(
                                      Cohorts,
                                      TimeP,
                                      lex.order = TRUE,
                                      sep = ", T="
                                    ),
                                    y = TBV
                                    , fill=Cohorts)
                                ) +
                                  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),legend.position = "topright", legend.text = element_text(size = 6.5)) +
                                  xlab("Cohorts, Time") +
                                  ylab("True Breeding Value") +
                                  geom_boxplot(
                                    width = 0.25,
                                    colour = "#3366FF",
                                    notch = T
                                  ) +
                                  theme(plot.background = element_rect(fill = "lightcyan", colour = "orange"))+
                                  ggtitle(paste("TBV ", trait[i]))
                              ),
                              list(i = i)
                            ))
                          ggplotsSelTBV[[i]] <- f2
                          
                          
                        }
                        for (i in 1:length(trait)) {
                          print(ggplotsSelTBV[[i]])
                        }
                        
                        ##
                        
                      }
                      #this is the thrid option to print TBV based on cohorts with repeats
                      else if (TBVplotOptions == 3) {
                        ##
                        View(uniqueRepeatsDF) #here it will show the users the index number of unique cohort classes
                        ###
                        
                        cat("Select cohort Repeats by the given index:\n--->To select type the index number and press Enter\n--->If you are done selecting press Enter twice")
                        
                        ##
                        y2<-uniqueRepeatsDF$uniqueRepeatsIndex
                        
                        #Here the user selection of cohorts received while checking for error input
                        while (TRUE){
                          selectedRepeatIndex<-scan()
                          selectedRepeatIndex<-unique(selectedRepeatIndex)
                          z2<-length(selectedRepeatIndex)
                          u2<-sum(is.element(selectedRepeatIndex, y2))
                          if(u2==z2){break}
                          else{
                            cat ("One or more index you typed is not correct.\nPlease try again by typing only the given index","\n")
                          }
                          
                        }
                        
                        
                        
                        
                        ##
                        
                        
                        
                        
                        
                        
                        
                        
                        # here it subset based on selected index
                        RepvectorSelected <- c(uniqueRepeatsDF$uniqueRepeats[selectedRepeatIndex])
                        
                        RepLstempDat <- list() #here initializing empty list
                        #For loop will create empty data frames inside the empty list
                        for (i in 1:length(trait)) {
                          assign(
                            trait[i],
                            data.frame(
                              TBV = c(),
                              Cohorts = c(),
                              TimeP = c(),
                              Traits = c(),
                              Cohort_Rept = c(),
                              Cohort_Rept_ID = c(),
                              Cohort_Rept_Com = c()
                            )
                          )
                          RepSelectedlst <- paste(trait[i], sep = "")
                          RepSelectedls <- get(paste(trait[i], sep = ""))
                          RepLstempDat[[RepSelectedlst]] <-RepSelectedls
                          
                        }
                        #For loops below are filtering out the selection
                        for (k in 1:length(trait)) {
                          #tempDat<-data.frame()
                          for (i in 1:length(RepvectorSelected)) {
                            Reptemp <-
                              subset(
                                listTraits[[k]],
                                RepvectorSelected[i] == listTraits[[k]]$Cohort_Rept_Com
                              )
                            RepLstempDat[[k]] <-
                              rbind(RepLstempDat[[k]], Reptemp)
                          }
                        }
                        
                        
                        
                        for (i in 1:length(trait)) {
                          RepLstempDat[[i]]$Cohorts <- with(RepLstempDat[[i]],
                                                            factor(Cohort_Rept_Com, levels = unique(Cohorts)))
                        }
                        
                        ggplotsSelRepTBV <- list() #empty list 
                        #the actual plot
                        for (i in 1:length(trait)) {
                          
                          f2 <-
                            eval(substitute(
                              ggplotly(
                                ggplot(
                                  data = RepLstempDat[[i]],
                                  mapping = aes(
                                    x = interaction(
                                      Cohorts,
                                      TimeP,
                                      lex.order = TRUE,
                                      sep = ", T="
                                    ),
                                    y =
                                      TBV
                                    , fill=Cohorts)
                                ) +
                                  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),legend.position = "topright", legend.text = element_text(size = 6.5)) +
                                  xlab("Cohorts, Time") +
                                  ylab("True Breeding Value") +
                                  geom_boxplot(
                                    width = 0.25,
                                    colour = "#3366FF",
                                    notch = T
                                  ) +
                                  theme(plot.background = element_rect(fill = "lightcyan", colour = "orange"))+
                                  ggtitle(paste("TBV ", trait[i]))
                              ),
                              list(i = i)
                            ))
                          ggplotsSelRepTBV[[i]] <- f2
                          
                          
                        }
                        for (i in 1:length(trait)) {
                          print(ggplotsSelRepTBV[[i]])
                        }
                        
                        ##
                      }
                      
                      break
                    }
                    else {
                      print ("Please type only 1 or 2 or 3")
                    }
                    
                    
                  }
                  
                }
                #conditional to plot average breeding value of repeated cohorts
                else if (BVoptions == 2) {
                  #embeded while loop to control user input
                  while (TRUE) {
                    cat(
                      "Type: 1 for ploting Avg.BV of all cohorts\nType: 2 for ploting Avg.BV of selected cohorts",
                      "\n"
                    )
                    AverageBVplotOptions <- readline("------------------------->:") #user input received here
                    #conditional to do task based on user input to either plot avg.bv of all cohorts or those with repeats
                    if (AverageBVplotOptions == 1 ||AverageBVplotOptions == 2) {
                      #conditional if the user selected to plot all cohort avg.bv
                      if (AverageBVplotOptions == 1) {
                        ##
                        
                        ggplotsAllAvg <- list() #initializing empty list to save plots of available traits
                        #for loop doing the ploting trait by trait
                        for (i in 1:length(trait)) {
                          #dev.new()
                          f3 <- eval(substitute(
                            ggplotly(
                              ggplot(
                                data = AverageBVList[[i]],
                                mapping = aes(
                                  x = TimeP,
                                  y = AverageBV,
                                  group = Cohort_Rept_Com,
                                  color = Cohort_Rept_Com
                                )
                              ) +
                                
                                geom_line(aes(
                                  TimeP, AverageBV
                                )) +
                                theme(axis.text.x = element_text(angle = 90, vjust = 0.5),legend.position = "topright", legend.text = element_text(size = 6.5)) +
                                xlab("Time") +
                                ylab("Average Breeding Value") +
                                geom_line(size=0.5) +
                                geom_point(mapping = aes(color = Cohort_Rept),alpha=0.75) +
                                theme(plot.background = element_rect(fill = "lightcyan", colour = "orange"))+
                                ggtitle(paste("Avg.BV ", trait[i]))
                            ),
                            list(i = i)
                          ))
                          ggplotsAllAvg[[i]] <- f3
                          
                        }
                        for (i in 1:length(trait)) {
                          print(ggplotsAllAvg[[i]])
                        }
                        
                        ##
                        
                      }
                      #conditional if the user choose to plot only selected cohorts with reapeats
                      else if (AverageBVplotOptions == 2) {
                        
                        
                        View (uniqueRepeatsDF) #The index of cohorts with repeats displayed for user
                        
                        cat("Select cohort by the given index:\n--->To select type the index number and press Enter\n--->If you are done selecting press Enter twice")
                        
                        ##
                        y3<-uniqueRepeatsDF$uniqueRepeatsIndex
                        #Here the user selection of cohorts received while checking for error input
                        while (TRUE){
                          AvgselectedCohortIndex<-scan()
                          AvgselectedCohortIndex<-unique(AvgselectedCohortIndex)
                          z3<-length(AvgselectedCohortIndex)
                          u3<-sum(is.element(AvgselectedCohortIndex, y3))
                          if(u3==z3){break}
                          else{
                            cat ("One or more index you typed is not correct.\nPlease try again by typing only the given index","\n")
                          }
                          
                        }
                        
                        
                        
                        
                        ##
                        
                        
                        #Here filtering out the selected repeated cohorts
                        AvgvectorSelected <- c(uniqueRepeatsDF$uniqueRepeats[AvgselectedCohortIndex])
                        
                        LsAvgtempDat <- list() #initializing an empty list to save data frames per trait of selected cohorts
                        #within the list creating empty data frames to save data of selected cohorts with repeats
                        for (i in 1:length(trait)) {
                          assign(
                            trait[i],
                            data.frame(
                              AverageBV = c(),
                              Cohorts = c(),
                              TimeP = c(),
                              Traits = c(),
                              Cohort_Rept = c(),
                              Cohort_Rept_ID = c(),
                              Cohort_Rept_Com = c()
                            )
                          )
                          AvgSelectedlst <- paste(trait[i], sep = "")
                          AvgSelectedls <- get(paste(trait[i], sep = ""))
                          LsAvgtempDat[[AvgSelectedlst]] <- AvgSelectedls
                          
                        }
                        
                        #The following 2 cohorts does the filtering out of selected cohorts and saving them
                        #to the LisAvgtempDat list to their respective data frames
                        for (k in 1:length(trait)) {
                          for (i in 1:length(AvgvectorSelected)) {
                            Avgtemp <-
                              AverageBVList[[k]][AverageBVList[[k]]$Cohort_Rept_Com == AvgvectorSelected[i], ]
                            LsAvgtempDat[[k]] <- rbind(LsAvgtempDat[[k]], Avgtemp)
                          }
                          
                        }
                        
                        
                        for (i in 1:length(trait)) {
                          LsAvgtempDat[[i]]$Cohorts <- with(LsAvgtempDat[[i]],
                                                            factor(Cohort_Rept_Com, levels = unique(Cohorts)))
                        }
                        
                        
                        ggplotsSelAvg <- list() #Initializing empty list to save plots
                        
                        #doing plots
                        for (i in 1:length(trait)) {
                          
                          f4 <- eval(substitute(
                            ggplotly(
                              ggplot(
                                data = LsAvgtempDat[[i]],
                                mapping = aes(
                                  x = TimeP,
                                  y = AverageBV,
                                  group = Cohort_Rept_Com,
                                  color = Cohort_Rept_Com
                                )
                              ) +
                                
                                theme(axis.text.x = element_text(angle = 90, vjust = 0.5),legend.position = "topright", legend.text = element_text(size = 6.5)) +
                                xlab("Time") +
                                ylab("Average Breeding Value") +
                                geom_line(size=0.5) +
                                geom_point(mapping = aes(color = Cohort_Rept),alpha=0.75) +
                                theme(plot.background = element_rect(fill = "lightcyan", colour = "orange"))+
                                ggtitle(paste(
                                  "Average BV ", trait[i]
                                ))
                            ),
                            list(i = i)
                          ))
                          ggplotsSelAvg[[i]] <- f4
                          
                        }
                        for (i in 1:length(trait)) {
                          print(ggplotsSelAvg[[i]])
                        }
                        break
                        
                        
                      }
                      
                      
                      break
                    }
                    else{
                      print("please type only 1 or 2")
                    }
                  }
                  
                  
                }
                break
              }
              
              else{
                print("Please type only 1 or 2")
              }
            }
            
          }
          #The following conditional harbours where breeding development accross cohorts or accross time ploted
          else if (as.numeric(choice) == 2) {
            #This is a trycatch statement to handle error that might happen due to the rdata and problems with 
            #sampling. It prevent the program from crushing. It it fails to plot inbreeding the user atleast 
            #be able to continue using the function such as plotting avg.bv or download data or get html summary
            tryCatch({
              #here the user asked to enter needed sample size for sampling IBD and HBD. The more number the user
              #type the slower the it will be
              cat("\nThe total population is: ", nrow(listTraits[[1]]),"\nPlease consider the larger the number of ibd and hbd sample the slower the estimation time.\nUsually ibd=100 to 200, hbd=100 to 200 show the development of inbreeding reasonably\n\n")
              
              cat("Enter the number of individual pairs to sample for kinship (IBD) calculation","\n")
              
              ibdSample<-readline("------------------------------>:") #ibdSamples received here
              #
              #This while loop is checking for the user input 
              #of ibdSample as number
              while(TRUE){
                if (suppressWarnings(is.na(as.numeric(ibdSample)))){
                  cat("Wrong input: What you entered is not a number!!!","\n")
                  cat("Enter only the number of individual pairs to sample for kinship (IBD) calculation","\n")
                  ibdSample<-readline("------------------------------>:")
                }else if (suppressWarnings(!is.na(as.numeric(ibdSample)))){break}
              }
              #
              cat("Enter the number of individuals to sample for homozygosity by descent (HBD) calculation","\n")
              hbdSample<-readline("------------------------------>:") #hbdSamples received here
              #
              #This while loop is checking for the user input 
              #of hbdSample as number
              
              while(TRUE){
                if (suppressWarnings(is.na(as.numeric(hbdSample)))){
                  cat("Wrong input: What you entered is not a number!!!","\n")
                  cat("Enter only the number of individuals to sample for homozygosity by descent (HBD) calculation","\n")
                  hbdSample<-readline("------------------------------>:")
                }else if (suppressWarnings(!is.na(as.numeric(hbdSample)))){break}
              }
              
              #Here it is just taking advantage of the MoBPS function to plot kinship development, using the number 
              #of ibd and hbd to sample that the user intered, but instead of using the plot this function is
              #interested to extract the data MoBPS calculated for the inbreeding development, so it saved the data
              #in kplo
              kplo <- kinship.development(
                population,
                gen = 1:max(as.numeric(cohorts$generation)),
                ibd.obs = as.numeric(ibdSample),
                hbd = as.numeric(hbdSample),
                display.cohort.name = T,
                display.time.point = T
              )
              dev.off() #Here preventing r from displaying plot
              
              #here The main inbreeding data extracted for the MoBPS saved to kp as data frame
              kp <- data.frame(kplo)
              
              # creating time points vector
              timep = c()
              for (i in 1:length(cohorts$generation)) {
                timePt <-
                  subset(cohorts$time.point,
                         as.numeric(cohorts$generation) == i)
                timep <- as.numeric(append(timep, unique(timePt)))
              }
              #creating Cohort grouping according to their generation and saving them to cohortp list
              cohortp = list()
              for (i in 1:length(cohorts$generation)) {
                cohortP <- subset(cohorts$name, as.numeric(cohorts$generation) == i)
                if (i > max(as.numeric(cohorts$generation))) {
                  break
                }
                cohortp <- append(cohortp, list(cohortP))
              }
              #sapply return cohortp elements to vector cohortGenGroup
              cohortGenGroup <- sapply(cohortp, paste, collapse = " ")
              
              #Here the main dataframe to make the inbreeding development accross time and generation
              #done. The data frame is inbredTab. Here all the previous individual vectors joined together 
              #to form this data frame
              inbredTab <- data.frame(
                Generations = 1:max(as.numeric(cohorts$generation)),
                TimeP = timep,
                CohortGen = cohortGenGroup,
                InbreedingCof = kp$X1
              )
              
              #Below are two plots Kinship development accross time and generations
              
              #Here the actual ggplot for inbreeding development across time happen
              f9 <- eval(substitute(ggplotly(ggplot(data = inbredTab, 
                                                    mapping = aes(
                                                      x = TimeP,
                                                      y = InbreedingCof,)) +
                                               geom_line(aes(
                                                 TimeP, InbreedingCof )) +
                                               theme(axis.text.x = element_text
                                                     (angle = 90, vjust = 0.5), 
                                                     legend.position = "topright", 
                                                     legend.text = element_text(size = 6.5)) +
                                               xlab("Time") +
                                               ylab("Kinship (f) / Inbreeding (F)") +
                                               geom_line(aes(color="Inbreeding development")) +
                                               geom_point(mapping = aes(color = CohortGen),alpha=0.75) +
                                               scale_colour_discrete(name="Cohorts grouped by time")+
                                               ggtitle(paste("Kinship development across Time"))+
                                               theme(plot.background = element_rect(
                                                 fill = "lightcyan", colour = "orange"))+
                                               geom_hline(aes(yintercept=0), 
                                                          alpha = 0.4, colour = "cyan",size=0.75)+
                                               geom_vline(aes(xintercept=0), 
                                                          alpha = 0.4, colour = "cyan",size=0.75)),
                                    list(i = i)
              ))
              print(f9) #here it plots
              
              #Here the actual ggplot for inbreeding development across generation happen
              f10 <- eval(substitute(ggplotly(ggplot(data = inbredTab,
                                                     mapping = aes(
                                                       x = Generations,
                                                       y = InbreedingCof,)) +
                                                geom_line(aes(
                                                  Generations, InbreedingCof)) +
                                                theme(axis.text.x = element_text(
                                                  angle = 90, vjust = 0.75), 
                                                  legend.position = "topright", 
                                                  legend.text = element_text(size = 6.5)) +
                                                xlab("Generations") +
                                                ylab("Kinship (f) / Inbreeding (F)") +
                                                geom_line(aes(color="Inbreeding development")) +
                                                geom_point(mapping = aes(color = CohortGen), alpha=0.5) +
                                                scale_colour_discrete(name="Cohorts grouped by generation")+
                                                ggtitle(paste("Kinship development accross Generations"))+
                                                theme(plot.background = element_rect(
                                                  fill = "lightcyan", colour = "orange"))+
                                                geom_hline(aes(yintercept=0), 
                                                           alpha = 0.4, colour = "cyan",size=0.75)+
                                                geom_vline(aes(xintercept=0), 
                                                           alpha = 0.4, colour = "cyan",size=0.75)),
                                     list(i = i)
              ))
              print(f10) #here it prints the plot
              
              #Below are the error and warning friendly messages skip crushing if 
              #something went wrong or if the trycatch found something to complain
            }, error = function(e){
              message(cat("An error occured, please check the following error\n"))
              
              message(e)
              message(cat("\nThe program is continuing to run\n"))
              
            },
            warning=function(e) {
              message(cat("Warning occured, please check the following errors/warning\n"))
              
              message(e)
              message(cat("\nThe program is continuing to run\n"))
              
            })
            
          }
          
          #This conditional give the user options to get the actual data to save it 
          #to the user computer either in the current working directory or allow the
          #user to specify a different path or folder to save the data. It checks if
          #the user has already saved data or if the user created the folder for
          #instance. It also has an error handling included in it if something
          #went wrong or an error happen out of user control, it prevent program crusing
          
          else if (as.numeric(choice) == 3) {
            
            #These are variables which will be used to check if user save or not create folder or not
            cout1<-0;cout2<-0;cout3<-0;folcout<-0;toexitfol<-0
            
            #Embedded while loop to control or insist user input or give option to exit
            #peacefully without crashing
            while(TRUE){
              
              cat("Continue getting data?\n") #Just simple question if the user is still interested to continue
              
              #and it give this option to continue or exit
              cat("Type: 1 to continue getting data\nType: 2 to exit", "\n")
              
              choiseSave<-readline("------------------------->:")#here the user input received
              
              #The conditional fulfilling user wish to continue or exit
              if (choiseSave == 1 || choiseSave == 2){
                
                #This conditional is when the user choose to continue
                if(choiseSave == 1){
                  cat("Please choose folder option to save the data\n") #Tells the user to choose further choices
                  
                  #Here are the choices
                  cat("Type: 1 to automatically create a new folder in your working directory\nType: 2 to speficy a different path","\n")
                  
                  choiseFolder<-readline("------------------------->:") #here the user choise received
                  
                  #embedding the following executions within the trycatch so that to catch errors
                  tryCatch({
                    
                    #Embedded wile loop to control user
                    while (TRUE){
                      
                      #conditional fulfilling user choise
                      if (choiseFolder == 1 || choiseFolder == 2){
                        
                        #Here is the conditional if the user give green light for the function to create
                        #new folder to his working directory
                        if (choiseFolder == 1){
                          
                          #here is creating a folder in the user working directory automatically based on
                          #formating Sys.time for uniqness of name, it also checks if the user already
                          #created a folder recently to avoid creating many folders
                          
                          #this conditional is when new folder created authomatically for the first time
                          if (folcout==0){
                            pth<-paste("Simu_Dat", format(Sys.time(), "%F_%s"), sep = "")
                            dir.create(pth)
                            print(paste("Folder ", pth, " created in your working directory", sep = ""))
                            p<-paste(pth, sep = "")
                            folcout = folcout+1
                          }
                          #this conditional is when the creation of folder is not for the first time
                          else if (folcout > 0){
                            #Embedded while loop to control user
                            while(TRUE){
                              #just notifying the user
                              cat("You already created a folder called ",pth, " before\n")
                              #here are the options for the user going forward
                              cat("Type: 1 to create new folder anyway\nType: 2 to stop creating new folder","\n")
                              toexitfol<-readline("------------------------->:")#here the user input received
                              
                              #This conditional is when the user do not want to create again new folder but want
                              #to save files in the already created folder, it just breaks the this while loop
                              if (toexitfol == 2){
                                break
                              }
                              #This conditional is when the user insist on creating new folder anyway
                              else if (toexitfol == 1){
                                pth<-paste("Simu_Dat", format(Sys.time(), "%F_%s"), sep = "")
                                dir.create(pth)
                                print(paste("Folder ", pth, " created in your working directory", sep = ""))
                                p<-paste(pth, sep = "")
                                break
                              } else{
                                #anything else insist on saying this to the 
                                #user until the user obliged and typed 1 or 2
                                print("Please type only 1 or 2") 
                              }
                            }
                          }
                          break
                        }
                        
                        #This conditional is if the user wish to specify his own path
                        #the user just type his path for example
                        #c:/Users/Goitom/Documents/New folder/New folder
                        #quotation is not necessary when typing path
                        else if (choiseFolder == 2){
                          cat("Please specify the path\n")
                          pth<-readline(prompt="Please type the path here without quotation: ")
                          #
                          cc<-0 #tracks the number of attempts
                          while(TRUE){
                            if (dir.exists(pth)){
                              
                              break
                            }else if (!dir.exists(pth)){
                              cat("Wrong path!!! Please try again.","\n")
                              cc<-cc+1
                              if (cc==7){
                                cat ("\nToo many attempts!!! The program now will give you an ugly error message.\nYou can continue using the program")
                                break}
                              cat("Please specify the correct path\n")
                              pth<-readline(prompt="Please type the path here without quotation: ")
                              
                            }
                            
                          }
                          
                          #
                          p<-paste(pth, sep = "")
                          print(paste("Data will be saved in the dirctory ", pth, sep = ""))
                          break
                        }
                      } else {
                        print ("Please type only 1 or 2") #else insist
                      }
                    }
                    
                    #The following while loop is to control the user and in it
                    #holds the actual codes to download or save the data to the
                    #already created folder in the above steps
                    while(TRUE){
                      
                      #Here the user told the options and which data to save
                      #once the user chooses a data the data saved to the folder
                      #imidiately and the user get notified where it was saved and
                      #what the file name is
                      cat("Type: 1 to get TBV data\nType: 2 to get Average BV data\nType: 3 to get development of inbreeding data\nType: 4 to exit","\n")
                      choiseToDownload <- readline("------------------------->:") #here the user choise received
                      
                      if (choiseToDownload == 1 || choiseToDownload == 2 
                          || choiseToDownload == 3|| choiseToDownload == 4){
                        #choise 1 the user get the TBV data, the program also check 
                        #if the user already saved the data before, the user has option
                        #to skip saving duplicate, or to overwrite
                        if (choiseToDownload == 1){
                          if (cout1 == 0){
                            for(i in 1:length(trait)){
                              write.table(listTraits[[i]], 
                                          file =paste(p, "/TBV_",trait[i],".txt", sep = ""), row.names = FALSE)
                              print(paste("File ", "TBV_",trait[i],".txt", " saved in ", p," folder", 
                                          sep = ""))
                            }
                            cout1 = cout1 + 1
                          }
                          else if (cout1 == 1){
                            while (TRUE){
                              cat("You already saved this file before\n")
                              cat("Type: 1 to either over-write in the existing folder or re-write in the new folder\nType: 2 to keep the file","\n")
                              toexit<-readline("------------------------->:")
                              if (toexit == 2){
                                break
                              }else if (toexit == 1){
                                for(i in 1: length(trait)){
                                  write.table(listTraits[[i]], 
                                              file =paste(p, "/TBV_",trait[i],".txt", sep = ""), 
                                              row.names = FALSE)
                                  print(paste("File ", "TBV_",trait[i],".txt", " saved in ", p,
                                              " folder", sep = ""))
                                }
                                break
                              } else (print("Please type only 1 or 2"))
                            }
                          }
                        }
                        
                        #This is when user choose to download average breeding value data
                        #this also check if the file already saved and give options for user
                        else if (choiseToDownload == 2){
                          if (cout2 == 0){
                            for (i in 1: length(trait)){
                              write.table(AverageBVList[[i]], 
                                          file = paste(p, "/Avg.BV_",trait[i],".txt", sep = ""), row.names = FALSE)
                              print(paste("File ", "Avg.BV_",trait[i],".txt", " saved in ",
                                          p, " folder", sep = ""))
                            }
                            cout2=cout2+1
                          }
                          else if (cout2 == 1){
                            while (TRUE){
                              cat("You already saved this file before\n")
                              cat("Type: 1 to either over-write in the existing folder or re-write in the new folder\nType: 2 to keep the file","\n")
                              toexit<-readline("------------------------->:")
                              if (toexit == 2){
                                break
                              }else if (toexit == 1){
                                for (i in 1:length(trait)){
                                  write.table(AverageBVList[[i]], 
                                              file = paste(p, "/Avg.BV_",trait[i],".txt", sep = ""), 
                                              row.names = FALSE)
                                  print(paste("File ", "Avg.BV_",trait[i],".txt", " saved in ",
                                              p," folder", sep = ""))
                                }
                                break
                              }else{
                                print("Please type only 1 or 2")
                              }
                            }
                          }
                        }
                        
                        #The following chunk of conditional is for the user to save the inbreeding development
                        #But since the data is generating from a sampling instance the usere is told to 
                        #enter number of ibd and hbd individuals to sample
                        #Also  checks if the file aready saved or if the user wish to overwirte
                        else if (choiseToDownload == 3){
                          
                          if (cout3==0){
                            cat("To create and save the development of inbreeding data please type the following parameters\n")
                            cat("\nThe total population is: ", nrow(listTraits[[1]]),"\nThe larger the number of ibd and hbd sample the slower the estimation time\nUsually ibd=100 to 200, hbd=100 to 200 show the development of inbreeding reasonably\n\n")
                            
                            cat("Enter the number of individual pairs to sample for kinship (IBD) calculation","\n")
                            
                            ibdSample<-readline("------------------------------>:") #ibdSamples received here
                            #
                            #This while loop is checking for the user input 
                            #of ibdSample as number
                            while(TRUE){
                              if (suppressWarnings(is.na(as.numeric(ibdSample)))){
                                cat("Wrong input: What you entered is not a number!!!","\n")
                                cat("Enter only the number of individual pairs to sample for kinship (IBD) calculation","\n")
                                ibdSample<-readline("------------------------------>:")
                              }else if (suppressWarnings(!is.na(as.numeric(ibdSample)))){break}
                            }
                            #
                            cat("Enter the number of individuals to sample for homozygosity by descent (HBD) calculation","\n")
                            hbdSample<-readline("------------------------------>:") #hbdSamples received here
                            #
                            #This while loop is checking for the user input 
                            #of hbdSample as number
                            
                            while(TRUE){
                              if (suppressWarnings(is.na(as.numeric(hbdSample)))){
                                cat("Wrong input: What you entered is not a number!!!","\n")
                                cat("Enter only the number of individuals to sample for homozygosity by descent (HBD) calculation","\n")
                                hbdSample<-readline("------------------------------>:")
                              }else if (suppressWarnings(!is.na(as.numeric(hbdSample)))){break}
                            }
                            
                            kplo<-kinship.development(population, gen = 1:max(as.numeric(cohorts$generation)), 
                                                      ibd.obs = as.numeric(ibdSample), hbd=as.numeric(hbdSample), 
                                                      display.cohort.name = T, display.time.point = T)
                            dev.off()
                            #The main inbreeding data extracted for the plots
                            kp<-data.frame(kplo)
                            
                            timep=c()
                            for (i in 1:length(cohorts$generation)){
                              timePt<-subset(cohorts$time.point, as.numeric(cohorts$generation) == i)
                              timep<-append(timep, unique(timePt))
                            }
                            
                            cohortp=list()
                            for (i in 1:length(cohorts$generation)) {
                              cohortP<-subset(cohorts$name, as.numeric(cohorts$generation) == i)
                              if(i>max(as.numeric(cohorts$generation))){
                                break
                              }
                              cohortp<-append(cohortp, list(cohortP))
                            }
                            
                            cohortGenGroup<-sapply(cohortp, paste, collapse=" ")
                            inbredTab<-data.frame(Generations=1:max(as.numeric(cohorts$generation)),
                                                  TimeP=timep, CohortGen=cohortGenGroup, InbreedingCof=kp$X1)
                            
                            write.table(inbredTab, file = paste(p,"/inbreeding_Dev.txt",sep = ""), row.names = FALSE)
                            print(paste("File ", "inbreeding_Dev.txt", " saved in ", p, " folder", sep = ""))
                            cout3<-cout3+1
                            
                          }
                          else if (cout3 == 1){
                            while (TRUE){
                              cat("You already saved this file before\n")
                              cat("Type: 1 to either over-write in the existing folder or re-write in the new folder\nType: 2 to keep the file","\n")
                              toexit<-readline("------------------------->:")
                              
                              if (toexit == 1){
                                
                                cat("To create and save the development of inbreeding data please type the following parameters\n")
                                
                                cat("\nThe total population is: ", nrow(listTraits[[1]]),"/nThe larger the number of ibd and hbd sample the slower the estimation time\nUsually ibd=100 to 200, hbd=100 to 200 show the development of inbreeding reasonably\n\n")
                                
                                cat("Enter the number of individual pairs to sample for kinship (IBD) calculation","\n")
                                
                                ibdSample<-readline("------------------------------>:") #ibdSamples received here
                                #
                                #This while loop is checking for the user input 
                                #of ibdSample as number
                                while(TRUE){
                                  if (suppressWarnings(is.na(as.numeric(ibdSample)))){
                                    cat("Wrong input: What you entered is not a number!!!","\n")
                                    cat("Enter only the number of individual pairs to sample for kinship (IBD) calculation","\n")
                                    ibdSample<-readline("------------------------------>:")
                                  }else if (suppressWarnings(!is.na(as.numeric(ibdSample)))){break}
                                }
                                #
                                cat("Enter the number of individuals to sample for homozygosity by descent (HBD) calculation","\n")
                                hbdSample<-readline("------------------------------>:") #hbdSamples received here
                                #
                                #This while loop is checking for the user input 
                                #of hbdSample as number
                                
                                while(TRUE){
                                  if (suppressWarnings(is.na(as.numeric(hbdSample)))){
                                    cat("Wrong input: What you entered is not a number!!!","\n")
                                    cat("Enter only the number of individuals to sample for homozygosity by descent (HBD) calculation","\n")
                                    hbdSample<-readline("------------------------------>:")
                                  }else if (suppressWarnings(!is.na(as.numeric(hbdSample)))){break}
                                }
                                
                                kplo<-kinship.development(population, gen = 1:max(as.numeric(cohorts$generation)), 
                                                          ibd.obs = as.numeric(ibdSample), hbd=as.numeric(hbdSample), 
                                                          display.cohort.name = T, display.time.point = T)
                                dev.off()
                                
                                #The main inbreeding data extracted for the plots
                                kp<-data.frame(kplo)
                                
                                
                                
                                
                                
                                timep=c()
                                for (i in 1:length(cohorts$generation)){
                                  timePt<-subset(cohorts$time.point, as.numeric(cohorts$generation) == i)
                                  timep<-append(timep, unique(timePt))
                                }
                                
                                cohortp=list()
                                for (i in 1:length(cohorts$generation)) {
                                  cohortP<-subset(cohorts$name, as.numeric(cohorts$generation) == i)
                                  if(i>max(as.numeric(cohorts$generation))){
                                    break
                                  }
                                  cohortp<-append(cohortp, list(cohortP))
                                }
                                
                                cohortGenGroup<-sapply(cohortp, paste, collapse=" ")
                                inbredTab<-data.frame(Generations=1:max(as.numeric(cohorts$generation)),
                                                      TimeP=timep, CohortGen=cohortGenGroup,
                                                      InbreedingCof=kp$X1)
                                
                                write.table(inbredTab, file = paste(p,"/inbreeding_Dev.txt",sep = ""),
                                            row.names = FALSE)
                                
                                print(paste("File ", "inbreeding_Dev.txt", " saved in ", p, " folder",
                                            sep = ""))
                                
                                break
                              }
                              else if (toexit == 2){
                                break
                              }
                              else{print("Please type only 1 or 2")
                              }
                            }
                          }
                        }
                        
                        #This conditional let the user to exit from the while loop when
                        #the user finish saving all data needed
                        else if (choiseToDownload == 4){
                          break
                        }
                      } else {
                        print ("Please type only 1 or 2 or 3 or 4")
                      }
                    }
                    #The following are errors and warning handled and avoiding program crush
                  }, error = function(e){
                    message(cat("An error occured, please check the following error\n"))
                    
                    message(e)
                    message(cat("\nThe program is continuing to run\n"))
                  }, warning=function(e) {
                    message(cat("An error occured, please check the following error/warning\n"))
                    
                    message(e)
                    message(cat("\nThe program is continuing to run\n"))
                  })
                }
                #This also allow user to exit the outer while loop
                else if (choiseSave == 2){
                  break
                }
              }
              else{
                print("Please type only 1 or 2")
              }
            }
          }
          
          
          #This is the last choice for the user which is to generate html report
          #The user will get html report summary
          else if (as.numeric(choice) == 4) {
            
            #The following step is generating plots f13,f33,f34 and f55 for
            #TBV of all cohorts, Average BV of all cohorts with repeats, Development
            #of inbreeding in time and in generations
            
            #TBV of all cohorts
            ggplotlist <- list()
            
            for (i in 1:length(trait)) {
              listTraits2e <- listTraits[[i]][order(listTraits[[i]]$TimeP), ]
              listTraits[[i]]$Cohorts <- factor(listTraits[[i]]$Cohorts,
                                                levels = unique(listTraits2e$Cohorts))
              
              f13 <- eval(substitute( ggplotly(ggplot(
                data = listTraits[[i]],
                mapping = aes(
                  x = interaction(
                    Cohorts,
                    TimeP,
                    lex.order = TRUE,
                    sep = ", T="),
                  y = TBV, fill=Cohorts)) +
                  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
                        legend.position = "topright",
                        legend.text = element_text(size = 6.5)) +
                  xlab("Cohorts, Time") +
                  ylab("True Breeding Value") +
                  geom_boxplot(
                    width = 0.25,
                    colour = "#3366FF",
                    notch = T
                  ) +
                  theme(plot.background = element_rect(fill = "lightcyan", colour = "orange"))+
                  ggtitle(paste("TBV ", trait[i]))
              ), 
              list (i = i)
              ))
              
              ggplotlist[[i]] <- f13
              
            }
            
            ##Avg.BV all cohorts
            ggplotsAllAvg <- list()
            
            for (i in 1:length(trait)) {
              
              f33 <- eval(substitute(ggplotly(ggplot(
                data = AverageBVList[[i]],mapping = aes(
                  x = TimeP,
                  y = AverageBV,
                  group = Cohort_Rept_Com,
                  color = Cohort_Rept_Com
                )
              ) +
                geom_line(aes(TimeP, AverageBV)) +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
                      legend.position = "topright", 
                      legend.text = element_text(size = 6.5))+
                xlab("Time") +
                ylab("Average Breeding Value") +
                geom_line(size=0.5) +
                geom_point(mapping = aes(color = Cohort_Rept),alpha=0.75) +
                scale_colour_discrete(name="Cohorts Repeats")+
                ggtitle(paste("Avg.BV ", trait[i]))+
                theme(plot.background = element_rect(fill = "lightcyan", colour = "orange"))
              ),
              list(i = i)
              ))
              
              ggplotsAllAvg[[i]] <- f33
              
            }
            
            #sampling step to plotting development of inbreeding for html summary
            
            tryCatch({
              cat("To create the inbreeding development plots for the Html summary,\nplease specify the following parameters of ibd and hbd.","\n")
              cat("\nThe total population is: ", nrow(listTraits[[1]]),"\nThe larger the number of ibd and hbd sample the slower the estimation time\nUsually ibd=100 to 200, hbd=100 to 200 show the development of inbreeding reasonably\n\n")
              
              cat("Enter the number of individual pairs to sample for kinship (IBD) calculation","\n")
              
              ibdSample<-readline("------------------------------>:") #ibdSamples received here
              #
              #This while loop is checking for the user input 
              #of ibdSample as number
              while(TRUE){
                if (suppressWarnings(is.na(as.numeric(ibdSample)))){
                  cat("Wrong input: What you entered is not a number!!!","\n")
                  cat("Enter only the number of individual pairs to sample for kinship (IBD) calculation","\n")
                  ibdSample<-readline("------------------------------>:")
                }else if (suppressWarnings(!is.na(as.numeric(ibdSample)))){break}
              }
              #
              cat("Enter the number of individuals to sample for homozygosity by descent (HBD) calculation","\n")
              hbdSample<-readline("------------------------------>:") #hbdSamples received here
              #
              #This while loop is checking for the user input 
              #of hbdSample as number
              
              while(TRUE){
                if (suppressWarnings(is.na(as.numeric(hbdSample)))){
                  cat("Wrong input: What you entered is not a number!!!","\n")
                  cat("Enter only the number of individuals to sample for homozygosity by descent (HBD) calculation","\n")
                  hbdSample<-readline("------------------------------>:")
                }else if (suppressWarnings(!is.na(as.numeric(hbdSample)))){break}
              }
              
              kplo<-kinship.development(population, gen = 1:max(as.numeric(cohorts$generation)), 
                                        ibd.obs = as.numeric(ibdSample), hbd=as.numeric(hbdSample), 
                                        display.cohort.name = T, display.time.point = T)
              dev.off()
              
              #The main inbreeding data extracted for the plots
              kp<-data.frame(kplo)
              
              timep=c()
              for (i in 1:length(cohorts$generation)){
                timePt<-subset(cohorts$time.point, as.numeric(cohorts$generation) == i)
                timep<-append(timep, unique(timePt))
              }
              
              cohortp=list()
              for (i in 1:length(cohorts$generation)) {
                cohortP<-subset(cohorts$name, as.numeric(cohorts$generation) == i)
                if(i>max(as.numeric(cohorts$generation))){
                  break
                }
                cohortp<-append(cohortp, list(cohortP))
              }
              
              cohortGenGroup<-sapply(cohortp, paste, collapse=" ")
              inbredTab<-data.frame(Generations=1:max(as.numeric(cohorts$generation)),
                                    TimeP=as.numeric(timep), CohortGen=cohortGenGroup, InbreedingCof=kp$X1)
              
              
              
              
              #Kinship development accross time
              
              f34 <- eval(substitute(ggplotly(ggplot(
                data = inbredTab,
                mapping = aes(
                  x = TimeP,
                  y = InbreedingCof,)) +
                  
                  geom_line(aes(
                    TimeP, InbreedingCof)) +
                  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), 
                        legend.position = "topright", legend.text = element_text(size = 6.5)) +
                  xlab("Time") +
                  ylab("Kinship (f) / Inbreeding (F)") +
                  geom_line(aes(color="Inbreeding development")) +
                  geom_point(mapping = aes(color = CohortGen),alpha=0.75) +
                  scale_colour_discrete(name="Cohorts grouped by time")+
                  ggtitle(paste("Kinship development accross Time"))+
                  theme(plot.background = element_rect(fill = "lightcyan", colour = "orange"))+
                  geom_hline(aes(yintercept=0), alpha = 0.4, colour = "cyan",size=0.75)+
                  geom_vline(aes(xintercept=0), alpha = 0.4, colour = "cyan",size=0.75)
              ),
              list(i = i)
              ))
              
              
              #Kinship development accross Generations
              
              f55 <- eval(substitute(ggplotly(ggplot(
                data = inbredTab,mapping = aes(
                  x = Generations,
                  y = InbreedingCof,)) +
                  
                  geom_line(aes(
                    Generations, InbreedingCof)) +
                  theme(axis.text.x = element_text(angle = 90, vjust = 0.75), 
                        legend.position = "topright", legend.text = element_text(size = 6.5)) +
                  xlab("Generations") +
                  ylab("Kinship (f) / Inbreeding (F)") +
                  geom_line(aes(color="Inbreeding development")) +
                  geom_point(mapping = aes(color = CohortGen), alpha=0.5) +
                  scale_colour_discrete(name="Cohorts grouped by generation")+
                  ggtitle(paste("Kinship development accross Generations"))+
                  theme(plot.background = element_rect(fill = "lightcyan", colour = "orange"))+
                  geom_hline(aes(yintercept=0), alpha = 0.4, colour = "cyan",size=0.75)+
                  geom_vline(aes(xintercept=0), alpha = 0.4, colour = "cyan",size=0.75)
              ),
              list(i = i)
              ))
              
            }, error = function(e){
              message(cat("An Error occured, causing the following\n"))
              message(e)
              print(cat("\nYou can continue using this program such as save files and plot\n"))
            }, warning = function(e){
              message(cat("An Error/Warning occured, causing the following\n"))
              message(e)
              print(cat("\nYou can continue using this program such as save files and plot\n"))
            })
            
            
            #The following part is just creating html tags to be inserted to the html summary page
            #The package htmltools and htmlTable are used to create html tags and tables respectively
            
            
            summarErro<-(htmltools::h3("***Not able to plot development of inbreeding accross generations and time, due to Error.***"))
            summarEndo<-(htmltools::h3("---   ---   End of Summary --- ---"))
            summarHint<-(htmltools::h4("It is recomended to view the html summary in your web browser or to save it manualy in your device for better viewing and sharing, by using the option: Export-->Save as Web Page"))
            summarDote<-(htmltools::h6("Date:",format(Sys.time(),"%d-%b-%y %H:%M:%S"), ", Time Zone:", Sys.timezone()))  
            summarHader<-(htmltools::h2("Summary of MOBPS derived Breeding Values, Cohorts, population parameters and Inbreeding development"))
            summar<- (htmltools::h4("The breeding simulation considered the",htmltools::code(length(trait)),"trait(s) listed in Table-1 below.", "There are 1 to", max(as.numeric(cohorts$generation)),"generations,"," where the total population in the simulated breeding program is", htmltools::code(nrow(listTraits[[1]])),   " individuals. The duration of the whole breeding is", max(as.numeric(cohorts$time.point))," Time Units.", "There are ", length(coht), " unique cohorts across", max(as.numeric(cohorts$generation)), " generations.","\n","There are", length(uniqueRepeatsDF$uniqueRepeats), " cohort classes as listed in Table-2.", "Among these cohort classes,",length(unique(Repeats_DF$Cohort_Rept_Com)), "cohort have repeat runs across generations or time (Table-3).","Plot and save options can further be used to examine the data in detail.","The summary also includes the following tables and plots.") )
            uniqueRepeatsDFcopy<-uniqueRepeatsDF
            colnames(uniqueRepeatsDFcopy)<-c("Serial Num ||", " Cohort class")
            repSumar<-data.frame(table(Repeats_DF), row.names = NULL)
            rownames(repSumar)<-c()
            repSumar<-subset(repSumar, repSumar$Freq>0)
            traitTable<-data.frame(Serial_Num<-c(1:length(trait)), Trait<-c(trait))
            colnames(traitTable)<-c("Serial Num ||", " Trait Name")
            colnames(repSumar)<-c("Cohorts Repeated ||", "Repeat count down ||", " Cohort class", " || Number of individuals")
            
            summarTrait<-htmlTable::htmlTable(traitTable, caption="Table 1: Traits considered", fill=TRUE, rnames = FALSE)
            summar1<- htmlTable::htmlTable(uniqueRepeatsDFcopy, caption = "Table 2: Unique classes of cohorts",fill=TRUE, rnames = FALSE)
            summar2<-htmlTable::htmlTable(repSumar, caption = "Table 3: Unique class of cohorts with repeats", fill=TRUE, rnames = FALSE)
            ?htmlTable::htmlTable
            
            #This conditional check if f34 and f55 plots successfully created by the inbreeding calculation of 
            #MoBPS and if returned it will include the plots in the html summary, 
            #and if something wrong or error happen and it was not possible to produced the two plots, then 
            #the conditional skip to the next conditional which is presenting the html summary without the
            #inbreeding development plots. This way a crush prevented
            if (exists("f34")&&exists("f55")){
              
              htmltools::html_print(list(summarDote,summarHader,summar,summarTrait,
                                         summar1,summar2,ggplotlist,ggplotsAllAvg,f34,f55,summarHint))  
              ?htmlTable::htmlTable
            }
            else
            {
              htmltools::html_print(list(summarDote,summarHader,summar,
                                         summar1,summar2,ggplotlist,ggplotsAllAvg,summarErro,summarHint))
            }
          }
          break
        } else
        {
          print("Please type 1 or 2 or 3 or 4")
        }
      }
      
    }
    
    
    else if (continue == 2) {
      break
    }
    else {
      print ("Please enter 1 or 2")
    }
  }
  
  
  
  
  
  
}

##############################################################################
######### END OF ENTERACTIVE USER INTERFACE FOR PLOTTING BV, DEVELOPMENT OF###
#########INBREEDING, GETTING DATA AND GETTING HTML BASED INTERACIGITY#########
##############################################################################

##########################End of the function#################################










##################Calling the function here in R Script below#################
do_plot()

###I tested the function supplying the below listed RData
#1 gtesfay_GenomicAssistedRecurrentSelection.RData

#2 gtesfay_MoBPS_Vis21_pseudoBVE_ (1).RData

#3 gtesfay_Sheep_breeding.RData

#4 gtesfay_simpleCattle.RData

#5 gtesfay_Cattle.RData

#6 gtesfay_cattleDairy.RData
