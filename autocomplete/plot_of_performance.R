source('autocomplete.R')
source('randomizedSamples.R')

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(microbenchmark))
# How performances Scales With Number of Words
# Uses random samples from movies file

random_sample_sizes <- function(n_times = 5) {
    
    # Evaluate expression using microbenchmark n_times (default is 5)
    file <- movies
    sample_sizes <- numeric(69)
    for (i in 1:69) {
        values <- c(1:6, seq(6, 12.3, 0.1)[-1])
        sample_sizes[i] <- exp(values[i])
    }

    time_at_samplesize <- vector(length = length(sample_sizes))
    # Mean run time over n_times evaluations
    for (i in 1:length(sample_sizes)) {
        sample_of_movies <- sample_n(file, sample_sizes[i])
        query <- 'the'
        sorted_movies <- sort_input(sample_of_movies$term)
        time_at_samplesize[i] <- 
            mean(microbenchmark(occurrences(query, sorted_movies), times = n_times)$time)
    }
    
    # Convert nanoseconds to Seconds
    time_at_samplesize_seconds <- time_at_samplesize * 1e-9
    fit <- lm(time_at_samplesize_seconds ~ log(sample_sizes))
    
    options(device = "png")
    png("performance.png")

    plot(sample_sizes, time_at_samplesize_seconds, xlab = "Number of Words", ylab = "Time in Seconds", 
         main = "Performance With Number of Words")
    curve(coef(fit)[1] + coef(fit)[2] * log(x), add = TRUE, col = 'red')
    
    dev.off()
    return(c(coef(fit)[1], coef(fit)[2]))
}