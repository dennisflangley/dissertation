## Make binary variable, recode 'na_vals' to NA
make_dummy <- function(var, on_vals, na_vals = NULL){
  case_when(
    is.na(var)       ~ NA_integer_,
    var %in% na_vals ~ NA_integer_,
    var %in% on_vals ~ 1L,
    TRUE             ~ 0L
  )
}

## Reverse order of a variable
reverse_code <- function(var, na_vals = NULL){
  var_nas <- ifelse(var %in% na_vals, NA_integer_, var)
  max_val <- max(var_nas, na.rm = TRUE)
  rev_var <-  as.integer(var_nas - max_val)  %>% abs()
  return(rev_var)
}

## Custom ggplot for visualization of missing data
## requires reshape2 library for melt function
library(reshape2)
ggplot_missing <- function(x){
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}

## Gives the percent of observations of a variable that are NA
percent_missing <- function(x){sum(is.na(x))/length(x)*100}