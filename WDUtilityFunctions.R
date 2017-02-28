# Function that prints versions of packages
printversion <- function(package_name){
    if(length(package_name) > 1){
        for(i in package_name){
            printversion(i)
        }
    }else{
        print(paste0(package_name, " version ", packageVersion(package_name)))
    }
}


plot_2_mle<- function(estim1, estim2,
                      xlab="",
                      main="",
                      legend1 = "L1",
                      legend2 = "L2",
                      legend3 = "L3",
                      scale_text=1.0,
                      legend_position="topright",
                      y.intersp = 1.5,
                      text.width_scale = 1.0,
                      trueValue=NA,
                      plot_legend=T,
                      xlim=NULL,
                      v3=0){
    
    get_hist <- function(estim){
        hist(estim[1], breaks=c(estim[1] - estim[2], estim[1] + estim[2]), plot=F) 
    }
    
    p1 = get_hist(estim1)
    p2 = get_hist(estim2)
    
    ymax = 1
    
    out_text = c(legend1, legend2)
    out_col  = c(rgb(0,0,1,1/4), rgb(1,0,0,1/4))
    out_lty  = c(2,2)
    out_lwd  = c(1,1)
    out_pch  = c(15,15)

    p3 = NULL
    if(length(v3) > 1){
        p3 <- hist(v3, breaks=breaks, plot=F) 
        p3$counts = p3$density
    }
    
    xxx = c(estim1[1] - estim1[2], estim1[1] + estim1[2], estim2[1] - estim2[2], estim2[1] + estim2[2], trueValue-estim1[2], trueValue+estim1[2])
    
    if(is.null(xlim)){
        xlim = c(min(xxx), max(xxx))
    }

    plot( p1, col=rgb(0,0,1,1/4), main=main, xlab=xlab, ylab="", ylim=c(0, 1.0), xlim=xlim, yaxt='n')  # first histogram
    plot( p2, col=rgb(1,0,0,1/4), add=T)  # second
    
    #axis(side = 1, at = x, labels = FALSE, tck = -0.01)
    
    abline(v = estim1[1],
          col = rgb(0,0,1,0.4),
          lwd = 1,
          lty = 2
    )

    abline(v = estim2[1],
          col = rgb(1,0,0,0.4),
          lwd = 1,
          lty = 2
    )
                
    if(!is.null(p3) ){
        plot(p3, lty=2, lwd = 15, type="l", add=T)  # second
        out_text = c(out_text, "Test sample")
        out_col  = c(out_col, 1)
        out_lty  = c(out_lty, 2)
        out_lwd  = c(out_lwd, 1)
        out_pch  = c(out_pch, NA)
    }
    
    if(!is.na(trueValue)){
        out_text = c(out_text, "True")
        out_col  = c(out_col, "red")
        out_lty  = c(out_lty, 3)
        out_lwd  = c(out_lwd, 3)
        out_pch  = c(out_pch, NA)
        abline(v = trueValue,
         col = "red",
         lwd = 3,
         lty = 3
        )
        
    }
    
    if(plot_legend){
        legend(x = legend_position, # location of legend within plot area
        out_text,
        col = out_col,
        lty = out_lty,
        pch = out_pch,
        lwd = out_lwd,
        cex =1.2 * scale_text,
        y.intersp = y.intersp, x.intersp = 0.5,
        text.width=  (max(xxx) - min(xxx)) * 0.25 * text.width_scale
        )    
    }
          
                              
}




plot_mle_distribution <- function(estim1, estim2=NA, trueValue=NA,
                              xlab="", scale_text=1.0, legend_position="topright", y.intersp = 1.5,
                              text.width_scale = 1.0){
    
    
    
    plot_poly <- function(estim, doplot, color){
        xmean = estim[1]
        xstdev = estim[2]
        
        xx = c(xmean - xstdev, xmean - xstdev, xmean + xstdev, xmean + xstdev)
        yy = c(-1, 2, 2, -1)
        x2 = c(xmean, xmean)
        y2 = c(0, 1)
        
        if(doplot){
            plot(x2, y2, type = "l", xlab=xlab, ylab="", ylim=c(0,1))
        }
        
        polygon(xx, yy, col = adjustcolor(color, alpha.f=0.2), border = FALSE)    
    }
    
    plot_poly(estim1, T, "royalblue")
    plot_poly(estim2, F, "green")
    
    abline(v = trueValue,
           col = "red",
           lwd = 2
    )
    
    xxx = c(xmean - xstdev, xmean + xstdev, trueValue)
    
    
    abline(v = estim1[1],
     col = "royalblue",
     lwd = 2)

    out_text = c("Mean+Stdev", "True")
    
    legend(x = legend_position, # location of legend within plot area
     out_text,
     col = c("royalblue", "red"),
     lwd = c(2, 2),
     #cex =1.2 * scale_text,y.intersp = y.intersp, x.intersp = 0.5,
     #text.width=  (max(xxx) - min(xxx)) * 0.25 * text.width_scale
     #text.width=  (max(xxx) - min(xxx)) * 0.25 * text.width_scale
    )    
     
     
}


plot_distribution <- function(distr, trueValue=NA, xlab="", scale_text=1.0, legend_position="topright", y.intersp = 1.5,
                              text.width_scale = 1.0){

    hist(distr, # histogram
     20,    
     col = "peachpuff", # column color
     border = "black", 
     prob = TRUE, # show densities instead of frequencies
     xlab = xlab,
     main = ""
    )
    
    lines(density(distr), # density plot
     lwd = 2, # thickness of line
     col = "chocolate3")

    abline(v = mean(distr),
     col = "royalblue",
     lwd = 2)
    
    out_text = c("Density plot", "Mean")
    if(!is.na(trueValue)){
        out_text = c("Density plot", "Mean", "True")
        
        abline(v = trueValue,
         col = "red",
         lwd = 2
        )
        
    }

    legend(x = legend_position, # location of legend within plot area
     out_text,
     col = c("chocolate3", "royalblue", "red"),
     lwd = c(2, 2, 2),
     cex =1.2 * scale_text,y.intersp = y.intersp, x.intersp = 0.5,
     text.width=  (max(distr) - min(distr)) * 0.25 * text.width_scale
     #text.width=  6
    )    
    print(summary(distr))
    d = as.data.frame(distr)
    colnames(d) = ""
    dd = describe(d)
    print(dd)
}


# Generate skew normal
gen_skew_normal <- function(N = 1000, omega = 5, alpha = 100.0, plot=F){
    loc = 0.0
    delta = alpha / sqrt(1+alpha**2)
    mmean = loc + omega * delta * sqrt(2/pi)

    test <- rsn(N, loc - mmean, omega, alpha)
    
    if(!plot){
        return(test)
    }
    plot_distribution(test)
}


simulate_person <- function(pars, id=NA){
    noise = gen_skew_normal(N     = 1,
                            omega = pars$Intercept_scale,
                            alpha = pars$Intercept_shape)
    
    out_df = data.frame(q = 1:pars$Nqtot)
    # Adding Quarter effect
    wd_prob_link = pars$Intercept + noise + (out_df$q - 1)* pars$q_coef
    
    wd_prob     = 1 / (1 + exp(-wd_prob_link))
    
    
    # Simulate withdrawals
    is_wd = rbinom(pars$Nqtot, 1, wd_prob)
    
    out_df$WD=is_wd
    out_df$wd_prob = wd_prob
    out_df$wd_prob_link = wd_prob_link    
    out_df$WD_numeric = as.numeric(as.character(out_df$WD))
    
    # Training dataset
    df_train = out_df[out_df$q <= pars$Nq,]
    
    # Simulated Number of withdrawals
    nwd = sum(df_train$WD_numeric)
    out_df$NWithdrawals = nwd
    
    if(nwd == 0){
        # If there is no withdrawal, then set QFirstWD to -1
        out_df$QFirstWD = -1
    }else{
        non_zero_q = df_train[df_train$WD_numeric == 1,]$q
        # First withdrawal quarter
        out_df$QFirstWD = non_zero_q[1]
    }

    if(! is.na(id)){
        out_df$person_id = id
    }
    
    # Right after the first withdrawal we want to increase withdrawal probability. Only if CJump is non-zero.
    if(abs(pars$CJump) > 0.0001){
        # Dataframe after the first withdrawal
        df_after_firstwd = subset(out_df, (q > QFirstWD) & (QFirstWD > 0))
        # If non-zero dataframe, regenerate withdrawals
        if(nrow(df_after_firstwd) > 0){
            wd_prob_link = df_after_firstwd$wd_prob_link + pars$CJump
            wd_prob      = 1 / (1 + exp(-wd_prob_link))
            # re-simulate withdrawals
            is_wd = rbinom(length(wd_prob), 1, wd_prob)
            
            df_after_firstwd$WD=is_wd
            df_after_firstwd$wd_prob = wd_prob
            df_after_firstwd$wd_prob_link = wd_prob_link    
            df_after_firstwd$WD_numeric = as.numeric(as.character(df_after_firstwd$WD))
            
            out_df[out_df$q > out_df$QFirstWD,] = df_after_firstwd
        }
    }
    
    out_df
}


simulate_people <- function(pars, Npeople=NA){
    
    if(!is.na(Npeople)){
        pars$Npeople = Npeople
    }
    
    df = simulate_person(pars, 1)
    for(iperson in 2:pars$Npeople){
        df2 = simulate_person(pars, iperson)
        df = rbind(df, df2)
    }
    df$id = 1:nrow(df)
    df
}

get_stan_input <- function(df, df_test=NULL){
    
    n_people = max(df$person_id)
    
    stan_input = list(
        Nq      = max(df$q),
        Npeople = n_people,
        WD      = df$WD_numeric
        #,GenerateNpeople = 100
        #,FirstWithdrawalQuarter = df$QFirstWD
    )
    
    if(!is.null(df_test)){
        stan_input$Nq_test = as.integer(nrow(df_test) / n_people)
        #stan_input$WD_test = df_test$WD_numeric
    }
    
    stan_input
}   
 
 
 
# Function which plots two histograms on the same plot 
plot_2_histograms <-function(v1,                        # Vector1 of values
                             v2,                        # Vector2 of values
                             breaks="Sturges",
                             xlab="",
                             main="",
                             legend1 = "L1",
                             legend2 = "L2",
                             legend3 = "L3",
                             scale_text=1.0,
                             legend_position="topright",
                             y.intersp = 1.5,
                             text.width_scale = 1.0,
                             trueValue=NA,
                             plot_legend=T,
                             xlim=0,
                             v3=0
                            )
{
    p1 = NA
    if(length(breaks) == 1 ){
        p1 <- hist(v1, breaks=breaks, plot=F) 
        breaks = p1$breaks
    }else{
        v1 = v1[v1 > min(breaks)]
        v1 = v1[v1 < max(breaks)]
        p1 <- hist(v1, breaks=breaks, plot=F) 
    }
    
    v2 = v2[v2 > min(breaks)]
    v2 = v2[v2 < max(breaks)]
    
    p2 <- hist(v2, breaks=breaks, plot=F) 
    
    p1$counts = p1$density
    p2$counts = p2$density

    ymax = max(c(p1$counts, p2$counts))
    
    if(length(xlim) < 2) {
        xlim=c(min(breaks), max(breaks))
    }
    
    out_text = c(legend1, legend2)
    out_col  = c(rgb(0,0,1,1/4), rgb(1,0,0,1/4))
    out_lty  = c(0,0)
    out_lwd  = c(0,0)
    out_pch  = c(15,15)

    p3 = NULL
    if(length(v3) > 1){
        p3 <- hist(v3, breaks=breaks, plot=F) 
        p3$counts = p3$density
    }

    plot( p1, col=rgb(0,0,1,1/4), main=main, xlab=xlab, ylab="Density", ylim=c(0, 1.1*ymax), xlim=xlim)  # first histogram
    plot( p2, col=rgb(1,0,0,1/4), add=T)  # second
    if(!is.null(p3) ){
        plot(p3, lty=2, lwd = 15, type="l", add=T)  # second
        out_text = c(out_text, "Test sample")
        out_col  = c(out_col, 1)
        out_lty  = c(out_lty, 2)
        out_lwd  = c(out_lwd, 1)
        out_pch  = c(out_pch, NA)
    }
    
    if(!is.na(trueValue)){
        out_text = c(out_text, "True")
        out_col  = c(out_col, "red")
        out_lty  = c(out_lty, 3)
        out_lwd  = c(out_lwd, 3)
        out_pch  = c(out_pch, NA)
        abline(v = trueValue,
         col = "red",
         lwd = 3,
         lty = 3
        )
        
    }
    
    if(plot_legend){
        legend(x = legend_position, # location of legend within plot area
        out_text,
        col = out_col,
        lty = out_lty,
        pch = out_pch,
        lwd = out_lwd,
        cex =1.2 * scale_text,
        y.intersp = y.intersp, x.intersp = 0.5,
        text.width=  (max(breaks) - min(breaks)) * 0.25 * text.width_scale
        )    
    }
}

 

#' @title Univariate Plots
#' 
#' @description 
#' 
#' Makes univariate plots of observed and predicted reponse, with options to include a by-group variable (poorly tested)
#' Options to calculate mean or median
#' 
#' Asymptotic 95% CIs are calculated for the observed data, appropriate to either statistic
#' 
#' An exposure (count) bar chart is included in light grey
#' 
#' A vertical axis is not included for the count data, but a dashed line is supplied as a reference
#' 
#' Continuous variables are automatically binned by round numbers
#' 
#' Categorical variables have several options for ordering, labels are rotated, and subsampled for easier visualization
#' 
#' Inlcudes a number of options for nicer plots.
#' 
#' Custom colors have not been explored yet
#' 
#' Observed  Predicted are coded by color in the plot
#' Unless you specify a By_Var, in which case you that gets color and Observed and predicted are coded by line type nad symbol
#' 
#'
#' @param Data.0  data.frame or data.table with your data 
#' @param X  Name of variable for which you want univariate plots
#' @param Observed  Name of varaible that contains the observed response 
#' @param Predicted  Name of variable that contains model predictions 
#' @param By_Var  Name of variable to group by for an interaction plot.
#'   If not null, then by groups get color and observed/predicted are coded by symbol and line type
#'   Defaults to NULL
#' @param Weight  Name of observation weights for calculating response in each bin
#'   If NULL, defaults to vector of 1s
#' @param Type  "Cat" for character or factor variables, "Cont" for numeric or integer
#'   If NULL, checks class(X)
#'   Important for axis teatment
#' @param Cut  Minimum number of observations in a category - will drop categories below this level
#'   Default = NULL keeps everything
#' @param Reorder  "Response" reorders categorical variables by calculated Observed statisitc in each group
#'   "Count" reorders by count
#'   Default is ordering of factor levels for factors, and alphabetic for character variables
#' @param Stat  "mean" or "median"
#'   n.a.s are dropped in calculation
#'   Defaults to "mean"
#' @param Bar  Count number that will be marked by a dashed line for the count histogram
#'   Defaults to NULL = no line
#' @param Count_Scale  The count whose level will be 75% of the y range on the plot
#'   Default is the max bar on the histgram
#' @param X_Lab  Label for x axis
#'   Default is the supplied X
#' @param Y_Lab  label for Y axis
#'   Default is "Repsonse"
#' @param X_Cut  Qunatile of data to use for the plot - use this to drop outliers
#'   Default is c(0,1) (i.e. all the data), Y_Lim = NULL, 
#' @param Title Title for the plot
#'   Currently not included
#'   Default is NULL = none
#' @param Key_Title Name for the key
#'   Default is ""  i.e. no name.
#' @param File_Name Name of file to save the plot (as .png)
#'   Default in NULL and does not save
#' @param Width  Width of the saved plot in inches
#'   Default = 4 if a file name is provided
#' @param Height Height in inches of the saved
#'   Default = 4
#' @param Label_Font Font size for tick labels and text annotation
#'   Default = 14
#' @param Axes_Font Font size for axis labels
#'   Default = 16
#' @param Object TRUE returns the ggplot object
#'   Default is FALSE
#' @param pause Pause between plots. Only when X=NULL. Then, we're plotting all variables   
#' @examples \dontrun{
#' Plot.Data.Response(Data.0 = Data,
#'                    X = as.character(Variables[i]),
#'                    Observed = "Y",
#'                    Predicted = "P",
#'                    By_Var = NULL,
#'                    Weight = NULL,
#'                    Type= NULL, 
#'                    Cut = 30,
#'                    Reorder = "Response",
#'                    Stat = "DenisMean",
#'                    Bar = 1000,
#'                    Count_Scale = 10000, 
#'                    X_Lab = new_names[i],
#'                    Y_Lab = "Premium  (Median)", 
#'                    X_Cut = c(0,0.99),
#'                    Y_Lim = c(0, 800), 
#'                    Title = NULL,
#'                    Key_Title = "",
#'                    File_Name = as.character(Variables[i]),
#'                    Width = 6,
#'                    Height = 6,
#'                    Label_Font = 14,
#'                    Axes_Font = 16,
#'                    Object = FALSE) 
#' }                    
#' @export
Plot.Data.Response <- function(Data.0, X, Observed, Predicted=NULL, By_Var = NULL, Weight = NULL, Type= NULL, 
                               Cut = NULL, Reorder = "No", Stat = "DenisMean", Bar = NULL, Count_Scale = NULL, 
                               X_Lab = NULL, Y_Lab = NULL, X_Cut = c(0,1), Y_Lim = NULL, 
                               Title = NULL, Key_Title = "",
                               File_Name = NULL, Width = 4, Height = 4, Label_Font = 14, Axes_Font = 16, LineSize=1, Object = FALSE, pause=T) 
{
  
 library(ggplot2)
 library(data.table)
 library(scales)
 library(reshape2)
 library(gridExtra)
 library(dplyr)
  
  # If X is null, we're going to plot
  if(is.null(X)){
    cols = colnames(Data.0)
    cols = cols[!(cols %in% c(Observed, Predicted))]
    for(icol in cols){
      Plot.Data.Response(Data.0, icol, 
                         Observed = Observed,
                         Predicted = Predicted,
                         By_Var = By_Var,
                         Weight = Weight,
                         Type = Type,
                         Cut=Cut,
                         Reorder=Reorder,
                         Stat=Stat,
                         Bar=Bar,
                         Count_Scale=Count_Scale,
                         X_Lab=X_Lab,
                         Y_Lab=Y_Lab,
                         X_Cut=X_Cut,
                         Y_Lim=Y_Lim,
                         Title=Title,
                         Key_Title=Key_Title,
                         File_Name=File_Name,
                         Width=Width,
                         Height=Height,
                         Label_Font=Label_Font,
                         Axes_Font=Axes_Font,
                         LineSize=LineSize,
                         Object=Object
      )
      if(pause)
        readkey()
    }
    return(NULL)
  }
  
  
  Response <- NULL
  
  # Indicator whether there is no prediction
  NO_PREDICTION_IND = F
  if(is.null(Predicted)){
    Predicted = paste0(Observed, "COPY")
    Data.0[[Predicted]] = Data.0[[Observed]]
    NO_PREDICTION_IND = T
  }
  
  
  # Standardize data type and variable names
  data.tmp <- data.table(Data.0) # make a data.table
  if (is.null(Weight)) {
    nr <- nrow(data.tmp)
    data.tmp[, Weight := rep(1, nr)]
    Weight = "Weight"
  } 
  Vars <- c(X, Observed, Predicted, Weight, By_Var)
  data.tmp <- subset(data.tmp, select = Vars) # grab the variables that we want to work with
  setnames(data.tmp, Vars[1:4], c("X", "Observed", "Predicted", "Weight")) # standardize the names
  if (!is.null(By_Var)) {
    setnames(data.tmp, By_Var, c("By_Var"))
    By_Var <- "By_Var"
  }
  X.tmp <- data.tmp[, X] # pull out the variable for the univariate
  if (is.null(Type)) Type <- class(X.tmp)
  if (Type %in% c("integer", "numeric")) Type <- "Cont"
  if (Type %in% c("character", "factor")) Type <- "Cat"
  if (!((Type == "Cat") | (Type == "Cont")))
  {
    print("Error")
    return
  }
  if (is.null(X_Lab)) X_Lab <- X
  if (is.null(Y_Lab)) Y_Lab <- "Response"
  
  # specify binning for continuous variables
  if (Type == "Cont") {
    X.Lim <- quantile(X.tmp, probs = X_Cut, na.rm = TRUE, names = FALSE) 
    X.tmp <- sort(unique(X.tmp))
    x.min <- X.Lim[1]
    x.max <- X.Lim[2]
    del <- x.max - x.min
    if (del > 0)
    {
      fact0 <- 10^(floor(log10(del)) - 1) # 1-2 power of 10 less than the distance we need to cover
      if (del/fact0 > 50) {
        fact0 <- fact0*5 # if that would make > 50 bins, mulitply by 5 so we have 10-20 bins
      } else {
        if ((del/fact0 > 25)) fact0 <- fact0*2 # if we have > 25 bins, multiply by 2 10 we have 10-25 bins
      } 
      if (length(X.tmp) > 40) { data.tmp[, X := fact0*round(X/fact0)] # If there are > 40 X values, bin them using our factor
        # the bind should be multiples of 10 or 5 so that the x-axis has round numbers
      } else fact0 <- min(diff(X.tmp)) # otherwise, still bin, but put 1 x value in each bin
    } else fact0 <- 1 # This should never happen
    bin.width <- 0.9*fact0
    bar.width <- 0.3*fact0 
    X.Lim[1] <- X.Lim[1] - bin.width
    X.Lim[2] <- X.Lim[2] - bin.width
  }
  
  # Make a table with the stats that we want for the plot (works for Cont and Cat)
  by_tmp <- c("X", By_Var)
  if (Stat == "Median") {
    Var.Table <- data.tmp[, list(Count = .N, 
                                 Observed = median(Observed), 
                                 Predicted = median(Predicted), 
                                 Lower = quantile(Observed, probs = qbinom(0.025, .N, 0.5, lower.tail = TRUE)/.N, na.rm = TRUE),
                                 Upper = quantile(Observed, probs = qbinom(0.025, .N, 0.5,lower.tail = FALSE)/.N, na.rm = TRUE)), 
                          by = by_tmp]
  }else if (Stat == "DenisMean") {
    Var.Table <- data.tmp[, list(Count = .N, 
                                 Observed = mean(Observed, na.rm = TRUE), 
                                 Predicted = mean(Predicted, na.rm = TRUE), 
                                 Lower = mean(Observed, na.rm = TRUE) - 1*sd(Observed)/sqrt(.N), 
                                 Upper = mean(Observed, na.rm = TRUE) + 1*sd(Observed)/sqrt(.N)
    ), 
    by = by_tmp]
  }else
  {
    Var.Table <- data.tmp[, list(Count = .N, 
                                 Observed = mean(Observed, na.rm = TRUE), 
                                 Predicted = mean(Predicted, na.rm = TRUE), 
                                 Lower = mean(Observed, na.rm = TRUE) - 2*sd(Observed)/.N, 
                                 Upper = mean(Observed, na.rm = TRUE) + 2*sd(Observed)/.N), 
                          by = by_tmp]
  }
  setkey(Var.Table, "X") # in both cases above we are calculating approximte 95% CIs
  
  # for Cat variables, drop low counts, and reorder factors, as desired
  if (Type == "Cat")
  {
    X.tmp <- as.character(Var.Table[, X]) # For factors
    ind.tmp <- (nchar(X.tmp)>25) # If the labels asre too long
    l.X.tmp <- strsplit(X.tmp, " ") # break them into words
    f <- function(x) paste(x[1:min(length(x),6)], collapse = " ") # only take the 1st 6
    X.tmp[ind.tmp] <- sapply(l.X.tmp, f)[ind.tmp]
    Var.Table[, X := X.tmp]
    
    if (!is.null(Cut)) Var.Table <- subset(Var.Table, subset = (Count > Cut)) # drop low counts
    setkey(Var.Table, X) # reset the key
    if (Reorder == "Response")  Var.Table <- Var.Table[order(-Predicted), ] 
    if (Reorder == "Count")  Var.Table <- Var.Table[order(-Count), ] 
    lvls <- Var.Table[,X] # use this to order the factor levels
    dm <- c("Low Count", "Missing", "NA") # these will be last, if they are in the table
    lvls <- c(setdiff(lvls, dm), intersect(lvls, dm))
    Var.Table[, X := factor(X, levels = lvls)] # This will make sure ggplot presents the categories in this order.
    # Of course missing categories will not be displayed
    bin.width <- 0.9
    bar.width <- 0.3
    x.min <- 1 - 0.5
    x.max <- length(lvls) + 0.5
  }
  
  # Reshape the table to stack Observed and Predicted
  id.vars <- c("X", "Lower", "Upper", "Count", By_Var)
  
  Data.Pred.Var <- data.table(melt(Var.Table, id.vars = id.vars, measure.vars = c("Observed", "Predicted"), 
                                   variable.name = c("Response"), value.name = c("Mean")))
  
  # If there is no prediction, then only Observed is present
  if(NO_PREDICTION_IND)
    Data.Pred.Var <- data.table(melt(Var.Table, id.vars = id.vars, measure.vars = c("Observed", "Observed"), 
                                     variable.name = c("Response"), value.name = c("Mean")))
  # Make some error bars
  # Data.Pred.Var[((Response == "Observed") & (Lower < 0)), Lower := 0] # Since we are usually plotting positive values, the lower bound should be positive
  Data.Pred.Var[Response == "Predicted", Upper := Mean] # For model predictions we don't have error bars
  Data.Pred.Var[Response == "Predicted", Lower := Mean]
  
  # Get y limits for a nicer plot
  # y.min <- 0.95*min(Data.Pred.Var[, Lower])
  if (is.null(Y_Lim))
  {
    y.min <- max(0.95*abs(min(Data.Pred.Var[, Upper])), 0.8*abs(min(Data.Pred.Var[, Mean])))*sign(min(Data.Pred.Var[, Mean]))
    y.max <- min(1.05*abs(max(Data.Pred.Var[, Upper])), 1.2*abs(max(Data.Pred.Var[, Mean])))*sign(max(Data.Pred.Var[, Mean]))
    Y_Lim <- c(0, y.max)
  } 
  y.max <- Y_Lim[2]
  y.min <- Y_Lim[1]  
  
  
  # scale the count to not overlap too much with the data
  Data.Pred.Var[, Scaled_Count := 0]
  if (is.null(Count_Scale)) Count_Scale <- max(Var.Table[, Count])
  peak <- 0.75*y.max/Count_Scale
  # don't let the histogram go past 75% Y axis or mean model prediction
  Data.Pred.Var[Response == "Observed", Scaled_Count := peak*Count]
  
  # Plot
  if (is.null(By_Var)) {p <- ggplot(Data.Pred.Var, aes(x = X, colour = Response)) 
  } else p <- ggplot(Data.Pred.Var, aes(x = X, colour = as.character(By_Var), shape = Response, linetype = Response)) 
  p <- p + ylab(Y_Lab) +  xlab(X_Lab) # if these was not set in the call, they are set to the response and the variable name that we are plotting
  p <- p + coord_cartesian(ylim = Y_Lim )
  if (Type == "Cont") p <- p + xlim(X.Lim) 
  if (Type == "Cat") {
    nl <- length(lvls)
    fct <- 1
    if (nl >= 40) fct <- round(nl/40) # Limit the catageory labels to 60
    dm <- seq.int(1, nl, fct) # where the labels will be
    p <- p + scale_x_discrete(breaks = lvls[dm], labels = lvls[dm]) 
    p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12)) 
    # this font could be made and argument.  But we pro bably don't want it bigger
  }
  # Make the line plot
  p <- p + geom_line(aes(x = as.numeric(X), y = Mean)) + geom_point(aes(x = as.numeric(X), y = Mean))  
  p <- p + geom_errorbar(aes(ymin = Lower, ymax = Upper), width = bar.width)
  # Make the histogram
  p <- p + geom_bar(aes(y = Scaled_Count), 
                    fill = alpha("black", 2/9), colour = "black", linetype = 0, 
                    position = "identity", stat = "identity", width = bin.width)                     
  if (!is.null(Bar))
  {
    mark <- peak*Bar # Position for a guid line to mark a count of Bar                    
    p <- p + geom_hline(yintercept = mark, colour="black", linetype = "longdash", size = 0.25) 
    p <- p + annotate("text", x = x.min + 0.1*(x.max - x.min), y = mark + (y.max - y.min)/40, label = paste("Count =", Bar, sep = " "))
  }
  
  # Formatting
  p <- p + theme(legend.title=element_blank(), 
                 legend.position = "top", 
                 legend.text = element_text(size=Label_Font), 
                 axis.text=element_text(size=Label_Font),
                 axis.title=element_text(size=Axes_Font))
  
  # output
  print(p)
  if(!is.null(File_Name))
  {
    png(paste(File_Name, ".png", sep = ""), width = Width, height = Height, units = "in", res = 600)
    print(p)
    dev.off()
  }
  if (Object) return(p)
}

 
 