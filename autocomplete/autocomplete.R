suppressPackageStartupMessages(library(assertthat))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(bit64))

# assumes file is data folder within working directory 
read_terms <- function(file_name) {
    file_path <- paste("./data", file_name, sep = "/")
    terms_df <- fread(file_path, skip = 1, header=FALSE)
    names(terms_df) <- c("weight", "term")
    
    # Check that correct number of terms were imported (as specified by first line)
    total_terms <- read.table(file_path, nrows = 1, header=FALSE)
    assert_that(nrow(terms_df) == as.numeric(total_terms))
    
    # Change words to lower case
    terms_df$term <- tolower(terms_df$term)
    
    # Remove punctuation
    terms_df$term <- gsub('[[:punct:]]', "", terms_df$term)
    
    return(terms_df)
}

sort_input<- function(values) {

    # Sorts vectors & data frames
    if (is.vector(values)) {
        values_sorted <- sort(values)
        assert_that(!any(is.unsorted(values_sorted)))
    } else if (is.data.frame(values)) {
        values_sorted <- values[sort(values$term, index.return=TRUE)$ix,]
        assert_that(!any(is.unsorted(values_sorted$term)))
    }
    return(values_sorted)
}

first_occurrence <- function(query, search_values) {
    # Takes Query Term and Sorted Vector of Values to Search Through
    assert_that(is.vector(search_values))
    # search values must be sorted
    
    first <- 0 
    last <- length(search_values)
    diff <- last - first 
    
    # Assert that each element between [first, last) exists
    assert_that(!any(is.na(search_values)))
    
    while (diff != 1) {
        rounded_mid <- ceiling((first + last) / 2)
        
        if (query <= search_values[rounded_mid]) {
            first <- first
            last <- rounded_mid
        } else if (query > search_values[rounded_mid]) {
            first <- rounded_mid
            last <- last
        }
        diff <- last - first
    }
    
    # Sets the location/index at which to check for query 
    if (first == 0 | 
        (query > search_values[first] && query <= search_values[last])) {
        location = last
    } else if (query > search_values[first] && query > search_values[last]) {
        return(NA)
    }

    # Checks whether the query is found at the index
    prefix_pattern <- paste("^", query, sep = "", collapse="")
    prefix_found <- grep(prefix_pattern, search_values[location])
    
    if (length(prefix_found) != 0) {
        return(location)
    } else {
        return(NA)
    }
}

occurrences <- function(query, search_values) {
    assert_that(is.vector(search_values))
    # search values must be sorted

    first_occ <- first_occurrence(query, search_values)
    # If first occurrence is found, initialize search to be (first, last]
    if (!is.na(first_occ)) {
        first <- first_occ
        last <- length(search_values) + 1
    } else {
        return(NA)
    }
    
    diff <- last - first
    
    # Define a match pattern
    match <- paste("^", query, sep = "", collapse="")

    while (diff != 1) {
        rounded_mid <- ceiling((first + last) / 2)
        
        # outputs 0 if match is not found and 1 if match is found 
        match_found <-  as.numeric(length(grep(match, search_values[rounded_mid]) != 0))
        
        if (match_found != 0) {
            first <- rounded_mid
            last <- last
        } else {
            first <- first
            last <- rounded_mid
        }
        
        diff <- last - first
    }
    
    # Check if there is a positive match at these indices
        match_at_first <- length(grep(match, search_values[first]))
        # If index for last is greater than length of vector
        if (last > length(search_values)) {
            match_at_last <- 0
        } else {
            match_at_last <- length(grep(match, search_values[last]))
        }
    
    if (match_at_first != 0 && match_at_last == 0) {
        last_occ <- first
    }
    assert_that(match_at_first != 0 && match_at_last == 0)
    
    # Returns vector in form (index of first occurrence, index last occurrence)
    return(c(first_occ, last_occ))
}

autocomplete <- function(query, search_df, n) {
    # Combines steps
    sorted_data <- sort_input(search_df)
    
    # Returns pair of lower and upper indices
    pair_indices <- occurrences(query, sorted_data$term)
    subset_by_query <- sorted_data[(pair_indices[1]:pair_indices[2]),]
    
    # Ordered by term weight (in decreasing order)
    order_by_weight <- subset_by_query[order(-subset_by_query$weight),]
    
    if (n >= nrow(order_by_weight)) {
        return(order_by_weight)
    } else {
        return(order_by_weight[(1:n),])
    }
}