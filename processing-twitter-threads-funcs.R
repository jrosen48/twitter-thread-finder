get_replies_recursive <- function(statuses) {
  statuses <- statuses[!is.na(statuses)]
  new_data <- rtweet::lookup_statuses(statuses)
  
  print(paste0("Accessed ", nrow(new_data), " new Tweets"))
  
  new_statuses <- new_data$reply_to_status_id[!is.na(new_data$reply_to_status_id)]
  
  if (length(new_statuses) > 0) {
    new_data_recursive <- get_replies_recursive(new_statuses)
    out_data <- bind_rows(new_data, new_data_recursive)
  } else {
    return(new_data)
  }
}

thread_finder <- function(status_id, d, out_statuses = NULL) {
  
  status_is_a_reply_to <- as.character(d[d$status_id == status_id, ]$reply_to_status_id)
  
  status_is_a_reply_to <- ifelse(length(status_is_a_reply_to) == 0, NA, status_is_a_reply_to)
  
  if (!is.na(status_is_a_reply_to)) {
    
    if (is.null(out_statuses)) {
      out_statuses <- c(status_id, status_is_a_reply_to)  
    } else {
      out_statuses <- c(out_statuses, status_is_a_reply_to)
    }
    
    thread_finder(status_is_a_reply_to, d, out_statuses)  
  } else {
    out_statuses
  }
}

remove_short_threads <- function(thread, d, i) {
  same_thread <- which(str_detect(d$thread_string, thread))
  
  same_thread_df <- d[same_thread, "thread_string"]
  
  same_thread_df <- same_thread_df %>%
    mutate(length_of_string = nchar(thread_string)) %>%
    arrange(desc(length_of_string))
  
  the_longest_thread <- pull(same_thread_df[1, "thread_string"])
  
  if (!(the_longest_thread == thread)) {
    remove_short_threads(the_longest_thread, d = d, i = i)
  } else {
    the_longest_thread
  }
}
