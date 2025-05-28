tfactor<-function(v){
  droplevels(as_factor(v,"both"))
}

lookfor<-function(name,dset){
  grep(name,names(dset),value=T)  
}

colPct<-function(tab){
  round(
    100*prop.table(tab,1),
    1
  )
}

rowPct<-function(tab){
  round(
    100*prop.table(tab,2),
    2  
  )
}

uniPct<-function(tab){
  round(
    100*prop.table(tab),
    2  
  )
}

# Get Unweighted frequency tables
ufreq <- function(x,dat) {
  # Create frequency table
  freq_table <- ftable(as.formula(paste0(x,"~1")),dat)
  
  # Calculate percentages
  percents <- uniPct(freq_table)
  
  # Create a dat frame with Label, Frequency, and Percentage (rounded to 1 digit)
  result <- data.frame(
    Label = levels(eval(parse(text=paste0("dat$",x)))),
    Frequencies = round(as.vector(freq_table), 1),
    Percent = round(as.vector(percents), 1)
  )
  names(result)[1]<-labs[[x]]

    # Return the result
  return(result)
}


vfreq <- function(var, dat) {
  # Load required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Please install the 'dplyr' package.")
  if (!requireNamespace("knitr", quietly = TRUE)) stop("Please install the 'knitr' package.")

  # Ensure variable exists
  if (!var %in% names(dat)) {
    stop(paste("Variable", var, "not found in the dataset."))
  }

  # Create frequency table
  freq_table <- dat %>%
    dplyr::group_by(.data[[var]]) %>%
    dplyr::summarise(
      Frequency = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Percent = round(Frequency / sum(Frequency, na.rm = TRUE) * 100, 1)
    ) %>%
    dplyr::arrange(dplyr::desc(Frequency))
        names(freq_table)[1]<-"Category"
        
        freq_table<-rbind(freq_table,
                          data.frame(
                            Category = "Total",
                            Frequency = sum(freq_table$Frequency),
                            Percent = round(sum(freq_table$Percent), 1)
                          )
        )
        
  # Print with nice formatting
  #knitr::kable(freq_table, align = "lrr", caption = paste("Frequency table for", var))
  knitr::kable(freq_table, align = "lrr")

}

# Get weighted frequency tables
wfreq <- function(var) {
  # Create frequency table
  freq_table <- svytable(as.formula(paste0("~", var,".f")), design = elsaw.s)
  
  # Calculate percentages
  percents <- uniPct(freq_table)
  
  # Create a data frame with Label, Frequency, and Percentage (rounded to 1 digit)
  result <- data.frame(
    Label = names(freq_table),
    Freq = round(as.vector(freq_table), 1),
    Pct = round(as.vector(percents), 1)
  )
  names(result)[1]<-attr(eval(parse(text=paste0("elsaw$",var))),"label")
  # Return the result
  return(result)
}

wfreq.r <- function(var) {
  # Create frequency table
  freq_table <- svytable(as.formula(paste0("~", var,".fr")), design = elsaw.s)
  
  # Calculate percentages
  percents <- uniPct(freq_table)
  
  # Create a data frame with Label, Frequency, and Percentage (rounded to 1 digit)
  result <- data.frame(
    Label = names(freq_table),
    Freq = round(as.vector(freq_table), 1),
    Pct = round(as.vector(percents), 1)
  )
  names(result)[1]<-attr(eval(parse(text=paste0("elsaw$",var))),"label")
  # Return the result
  return(result)
}

wfreq2 <- function(var, dat.s) {
  # Load required packages
  if (!requireNamespace("survey", quietly = TRUE)) stop("Please install the 'survey' package.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Please install the 'dplyr' package.")
  if (!requireNamespace("knitr", quietly = TRUE)) stop("Please install the 'knitr' package.")
  
  # Ensure variable exists
  if (!var %in% names(dat.s$variables)) {
    stop(paste("Variable", var, "not found in the survey design object."))
  }

  fmla <- as.formula(paste("~", var))
  
  # Compute weighted totals
  freq <- survey::svytable(fmla, design = dat.s)
  freq_df <- as.data.frame(freq)
  names(freq_df) <- c(var, "Frequency")
  
  # Add percentages
  freq_df <- freq_df %>%
    dplyr::mutate(
      Percent = round(100 * Frequency / sum(Frequency), 1)
    ) %>%
    dplyr::arrange(dplyr::desc(Frequency))
  
  # Print nicely
#  knitr::kable(freq_df, align = "lrr", caption = paste("Weighted frequency table for", var))
  knitr::kable(freq_df, align = "lrr")
  
}