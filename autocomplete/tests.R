source('autocomplete.R')
source('randomizedSamples.R')

suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(microbenchmark))

# Tests binary search algorithm using randomized rows (100 simulations each)
test_that("output from binary search is the same as that from linear search", {
    # Uses random sample of data and random query
    
    # Checks when search values has an even number of elements
    expect_true(tests_randomSamples(random_sample_files, 100))
    expect_true(tests_randomSamples(random_character_strings, 100))

    # Odd number of elements
    expect_true(tests_randomSamples(random_sample_files, 99))
    expect_true(tests_randomSamples(random_character_strings, 99))
})

test_that("returns NA when query < all search_values or query > all search values", {

    out_of_lowerBounds <- function(df, n_sim) {
        # n simulations
        returns_NA <- vector(length = n_sim)
        for (i in 1:n_sim) {
            random_sample <- sample_n(df[which(df$term >= 'e'),], 100)
            random_query <- sample_n(df[which(df$term < 'e'),], 1)$term
            sorted_values <- sort_input(random_sample$term)
            returns_NA[i] <- is.na(occurrences(random_query, sorted_values))
        }
        return(all(returns_NA == TRUE))
    }
    
    out_of_upperBounds <- function(df, n_sim) {
        # n simulations
        returns_NA <- vector(length = n_sim)
        for (i in 1:n_sim) {
            random_sample <- sample_n(df[which(df$term <= 'e'),], 100)
            random_query <- sample_n(df[which(df$term > 'e'),], 1)$term
            sorted_values <- sort_input(random_sample$term)
            returns_NA[i] <- is.na(occurrences(random_query, sorted_values))
        }
        return(all(returns_NA == TRUE))
    }
    
    # 100 simulations for different files
    expect_true(out_of_lowerBounds(movies, 100))
    expect_true(out_of_lowerBounds(wiktionary, 100))
    
    expect_true(out_of_upperBounds(babyNames, 100))
    expect_true(out_of_upperBounds(pokemon, 100))
})

test_that("mean run time for binary search < mean run time for linear search", {
    
    runTime <- function(randomizer_function, length_of_sample) {
        query <- randomizer_function(length_of_sample)$query 
        search_values <- randomizer_function(length_of_sample)$search_values
        
        if (is.data.frame(search_values)) {
            search_values <- search_values$term
        }
        search_values <- sort_input(search_values)
        
        # Default number of evaluations is 100
        binary_search_time <- microbenchmark(occurrences(query, search_values))$time
        linear_search_time <- microbenchmark(linear_search(query, search_values))$time
        return(mean(binary_search_time) < mean(linear_search_time))
    }

    expect_true(runTime(random_sample_files, 100))
    expect_true(runTime(random_character_strings, 100))
})

test_that("binary search handles matches at first and last indices of vector correctly", {
    # First occurrence of query is first element of vector of search values
    first_element <- function(df) {
        # 100 simulations 
        check_true <- vector(length = 100)
        for (i in 1:100) {
            simulations <- sample_n(df, 100)
            simulations <- sort_input(simulations$term)
            query <- min(simulations)
            check_true[i] <- first_occurrence(query, simulations) == 1
        }
        return(all(check_true == TRUE))
    }
    
    # Last occurrence of query is last element of vector of search values
    last_element <- function(df) {
        # 100 simulations 
        check_true <- vector(length = 100)
        for (i in 1:100) {
            simulations <- sample_n(df, 100)
            simulations <- sort_input(simulations$term)
            query <- max(simulations)
            check_true[i] <- occurrences(query, simulations)[2] == 100
        }
        return(all(check_true == TRUE))
    }
    
    expect_true(first_element(pokemon))
    expect_true(first_element(movies))
    
    expect_true(last_element(wiktionary))
    expect_true(last_element(babyNames))
})

test_that("first and last occurrences are equal if query/prefix is only found once", {
    first_equals_last <- function(df) {
        check_equality <- vector(length = 100)
        # 100 simulations
        for (i in 1:100) {
            simulations <- sample_n(df, 100)
            unique_values <- sapply(simulations$term, function(x) {length(grep(x, simulations$term))})
            unique_words <- names(unique_values[which(as.vector(unique_values) == 1)])
            
            query <- sample(unique_words, 1)
            simulations <- sort_input(simulations$term)
            
            indices_match <- occurrences(query, simulations)
            check_equality[i] <- (indices_match[1] == indices_match[2])
        }
        return(all(check_equality == TRUE))
    }
    
    expect_true(first_equals_last(pokemon))
    expect_true(first_equals_last(movies))
    expect_true(first_equals_last(babyNames))
    expect_true(first_equals_last(wiktionary))
})

test_that("when query & all elements in vector are equal, 
          first occurrence is first index, last occurrence is last index", {
    elements_equal_query <- function(df) {
        all_equal <- vector(length = 100)
        for (i in 1:100) {
            # 100 simulations
            simulations <- rep(sample_n(df, 1)$term, 100)
            query <- simulations[1]
            indices <- occurrences(query, simulations)
            all_equal[i] <- (indices[1] == 1 && indices[2] == 100)
        }
        return(all(all_equal) == TRUE)
    }
    expect_true(elements_equal_query(pokemon))
    expect_true(elements_equal_query(wiktionary))
    expect_true(elements_equal_query(babyNames))
    expect_true(elements_equal_query(movies))
})

test_that("when all elements in vector are the same, 
          but query is not equal, function returns NA", {
    elements_notEqual_query <- function(df) {
        all_na <- vector(length = 100)
        for (i in 1:100) {
            # 100 simulations
            sample_index <- sample(1:100, 1)
            simulations <- rep(df$term[1], 100)
            query <- sample(df$term[-sample_index], 1)
            
            diff <- length(grep(query, simulations))
            
            while (diff != 0) {
                query <- sample(df$term[-sample_index], 1)
                diff <- length(grep(query, simulations))
            }
            
            indices <- occurrences(query, simulations)
            all_na[i] <- is.na(indices)
        }
        return(all(all_na == TRUE))
    }
    
    expect_true(elements_notEqual_query(pokemon))
    expect_true(elements_notEqual_query(wiktionary))
    expect_true(elements_notEqual_query(babyNames))
    expect_true(elements_notEqual_query(movies))
})