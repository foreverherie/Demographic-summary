###defining the function 

summarize_demographic_distribution <- function(data, demographic_variable, status_column, group_column) {
  
  # Ensure demographic_variable and status_column are single character strings
  if (is.null(demographic_variable) || length(demographic_variable) != 1) {
    stop("Error: demographic_variable should be a single string.")
  }  
  if (is.null(status_column) || length(status_column) != 1) {
    stop("Error: status_column should be a single string.")
  }
  if (is.null(group_column) || length(group_column) != 1) {
    stop("Error: group_column should be a single string.")
  }
  
  # Dynamically refer to the columns passed as arguments
  #modify this to the groups/collumns you are truing to compare (in this case it is status and group collumns )
  data %>%
    group_by(!!sym(demographic_variable)) %>%
    summarise(
      positive_count = sum(!!sym(status_column) == "positive" & !!sym(group_column) == "GroupA", na.rm = TRUE),
      negative_count = sum(!!sym(status_column) == "negative" & !!sym(group_column) == "GroupA", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      positive_count = paste(
        positive_count, 
        "(", 
        round(positive_count / sum(positive_count) * 100, 1), 
        "%)", sep = ""
      ),
      negative_count = paste(
        negative_count, 
        "(", 
        round(negative_count / sum(negative_count) * 100, 1), 
        "%)", sep = ""
      )
    ) %>%
    pivot_longer(
      cols = c(positive_count, negative_count),
      names_to = "status",
      values_to = "Summary"
    ) %>%
    mutate(
      variable = demographic_variable
    ) %>%
    select(variable, !!sym(demographic_variable), status, Summary)
}

##create a summary list that will store the summarized counts and proportions 
summary_list <- list()


##list the (demographic) variables you are interested in 
demographic_variables <- c("age", "occupation")

# Loop over demographic variables, summarize each, and reshape
for (var in demographic_variables) {
  summary <- summarize_demographic_distribution(tested_clean, var) %>%
    pivot_wider(names_from = pfstatus, values_from = Summary) %>%
    mutate(variable = var) %>%
    rename(category = !!sym(var))%>%
    mutate(category = as.character(category)) 

  ##put the summaries in the created summary list   
  summary_list[[var]] <- summary
}

# Combine all summaries into one dataframe
combined_data <- bind_rows(summary_list) %>%
  relocate(variable, .before = category)

ptint (combined_data)