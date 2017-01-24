source('autocomplete.R')
source('randomizedSamples.R')

# Sort Before Running Binary Search
sorted_movies <- sort_input(movies$term)

microbenchmark(linear_search("the", sorted_movies))
microbenchmark(occurrences("the", sorted_movies))

microbenchmark(linear_search("an", sorted_movies))
microbenchmark(occurrences("an", sorted_movies))

microbenchmark(linear_search("un", sorted_movies))
microbenchmark(occurrences("un", sorted_movies))

profvis({
    sorted_movies <- sort_input(movies$term)
    occurrences("un", sorted_movies)
    occurrences("the", sorted_movies)
})