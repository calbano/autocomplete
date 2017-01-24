source('autocomplete.R')
suppressPackageStartupMessages(library(dplyr))

movies <- read_terms('movies.txt')
babyNames <- read_terms("baby-names.txt")
pokemon <- read_terms("pokemon.txt")
wiktionary <- read_terms("wiktionary.txt")

# Linear Search Function
linear_search <- function(query, search_values) {
    # Input query value and vector
    assert_that(is.vector(search_values))
    
    sorted_data <- sort_input(search_values)
    pattern <- paste("^", query, sep = "")
    
    n <- length(search_values)
    match_found <- vector(length = n)
    for (i in 1:n) {
        match_found[i] <- length(grep(pattern, sorted_data[i]))
    }
    
    indices_found <- which(match_found > 0)
    
    # Check that it is sorted in increasing order
    #assert_that(!is.unsorted(indices_found))
    return(c(indices_found[1], indices_found[length(indices_found)]))
}

# Chooses random samples of n rows from provided files
random_sample_files <- function(nrows_of_sample) {
    
    # Randomly choose one of the four test files
    files <- c('movies.txt', "baby-names.txt", "pokemon.txt", "wiktionary.txt")
    sample_file <- sample(files, 1)
    file <- read_terms(sample_file)
    
    # Random sample of n terms
    sample_df <- sample_n(file, nrows_of_sample)
    
    # Choose prefix randomly from list of common prefixes
    prefix_list <- c('anti', 'de', 'dis', 'en', 'em', 'fore', 'in', 'im', 
                     'inter', 'ir', 'ill', 'mis', 'mid', 'non', 'over', 'pre', 
                     're', 'the', 'semi', 'sub', 'super', 'the', 'un', 'under')
    
    rand_prefix <- sample(prefix_list, 1)
    return(list(search_values = sample_df, query = rand_prefix))
}

# Generates random character strings
random_character_strings <- function(length_of_sample) {
    # generates `length_of_sample` random terms and 1 random prefix
    
    terms <- vector(length = length_of_sample)
    for (i in 1:length_of_sample) {
        # Generates term by randomly sampling letters
        number_of_letters <- sample(1:10, 1)
        sample_of_letters <- sample(letters, number_of_letters)
        sample_term <- paste(sample_of_letters, collapse="")
        terms[i] <- sample_term
    }
    
    # Random 3-letter prefix
    sample_prefix <- sample(letters, 3)
    prefix <- paste(sample_prefix, collapse="")
    return(list(search_values = terms, query = prefix))
}

# Compares Output from Binary search and Linear Search with Random Input

tests_randomSamples <- function(randomizer_function, length_of_sample) {
    # Takes input from either of the 2 randomizer function to generate random data
    # Sample has n terms
    
    # 100 simulations
    matching_indices <- vector(length = 100)
    
    for (i in 1:100) {
        search_values <- randomizer_function(length_of_sample)$search_values
        query <- randomizer_function(length_of_sample)$query
        
        # Sort
        if (is.data.frame(search_values)) {
            search_values <- sort(search_values$term)
        } else if (is.vector(search_values)) {
            search_values <- sort(search_values)
        }
        
        assert_that(is.vector(search_values))

        bs_indices <- occurrences(query, search_values)
        ls_indices <- linear_search(query, search_values)
        
        # Returns TRUE if indices match
        match <- all((bs_indices == ls_indices)  |  (is.na(bs_indices) & is.na(ls_indices)))
        matching_indices[i] <- match
    }
    return(all(matching_indices == TRUE))
}