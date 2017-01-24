# Autocomplete Project 


### autocomplete.R

Contains 5 functions: 
1. *read\_terms* - assumes file is found in ./data
subfolder 

2. *sort\_input* - sorts a vector of values alphabetically 

3. *first\_occurrence* - finds first occurrence of a query 

4. *occurrences* - returns a vector in form (first\_occurrence, last\_occurrence).  
Sort and save sorted object before running function. 

5. *autocomplete* - combines steps and produces n occurrences of the found query, sorted in
decreasing order by weight

### randomizedSamples.R

Contains 4 functions: 

1. *linear\_search* - returns a vector of first
and last occurrence (in same form as `occurrences` function) by
performing a linear search 

2. *random\_sample\_files* - generates a
random sample of length n from a randomly selected file and a randomly
selected prefix 

3. *random\_character\_strings* - generates a random
sample of length n of randomly generated character strings and a
randomly generated prefix (by pasting random samples of letters) 

4. *randomized\_sample* - compares indices found from linear search to
those found from `occurrences` (binary search) using 100 randomized
samples, each of length n. Sample is generated randomly from specified
function - either `random_sample_files` or `random_character_strings`

### tests.R

Contains test suite, including the randomized tests (by sourcing
`randomizedSamples.R` and `autocomplete.R`)

### runTime.R

Contains expressions to determine run time for linear search function -
`linear_search` and binary search function `occurrences`for largest file
- movies.txt. 

**Note: Sort and save sorted file (data.frame) before
inputting into either search function)**

### plot\_of\_performance.R

Function generates random samples of movies.txt of different sample
sizes. For each sample size, the average run time of `occurrences` for
n-evaluations is recorded. Then, to assess performance, a plot (.png) is
created of run-time against sample size.

**See working directory to find the plot.**

### commandDriver.R

Command line program to run autocomplete function from the terminal.
Arguments that should be provided are: query (character string), file
name (i.e. movies.txt), n (number of query matches found, sorted in
decreasing order by weight)

For example: **Rscript commandDriver.R "the" baby-names.txt 5**
