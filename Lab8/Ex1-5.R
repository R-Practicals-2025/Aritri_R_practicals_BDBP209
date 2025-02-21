#Ex1-Given an integer, write a function to find if the integer is a palindrome.
is_palindrome <- function(num) {
  num_str <- as.character(num)  # Convert the integer to a string
  return(num_str == paste(rev(strsplit(num_str, NULL)[[1]]), collapse = ""))
}
is_palindrome(12321)
is_palindrome(12345)


#Ex2-Slice the string ‘seemerightnow’ to produce the following substrings: (a) ‘see’ (b) ‘me’(c) ‘right’
string <- "seemerightnow"
substring1 <- substr(string, 1, 3)
substring2 <- substr(string, 4, 5)  
substring3 <- substr(string, 6, 10)  
print(substring1)
print(substring2)
print(substring3)

#Ex3-Determine the fraction of G and C bases in the sequence “ATTGCGCATAGTCCGGG”.
library(stringr)
sequence <- "ATTGCGCATAGTCCGGG"
gc_fraction <- function(seq) {
  gc_count <- str_count(seq,pattern="GC")
  return(gc_count / nchar(seq))
}
gc_fraction(sequence)

#Ex4-Write a function to determine if a DNA nucleotide sequence is a palindrome in the
#sense that it is equal to its own complementary sequence read backward. For example,
#the sequence “TGGATCCA” is palindromic because its complement is “ACCTAGGT”
#which is same as the original sequence backward. The complementary base pairs are (A,T) and (G,C).
complement <- function(seq) {
  complement_map <- c(A = "T", T = "A", G = "C", C = "G")
  return(paste(complement_map[strsplit(seq, NULL)[[1]]], collapse = ""))
}
is_dna_palindrome <- function(seq) {
  rev_complement <- complement(seq)
  return(rev_complement == paste(rev(strsplit(seq, NULL)[[1]]), collapse = ""))
}
is_dna_palindrome("TGGATCCA")  
is_dna_palindrome("ATTGCGCATAGT")

#Ex5-Write a code to search and print the largest word in this sentence: ”She sells hundreds
#of sea oysters on the sea shore.” By extension, print the second largest word in the same
#sentence. Extend this code to allow searching for words of equal length and gathering them into a list.
find_largest_words <- function(sentence) {
  words <- unlist(strsplit(sentence, " "))
  word_lengths <- nchar(words)
  max_length <- max(word_lengths)
  second_max_length <- max(word_lengths[word_lengths < max_length])
  largest_words <- words[word_lengths == max_length]
  second_largest_words <- words[word_lengths == second_max_length]
  return(list(largest_words = largest_words, second_largest_words = second_largest_words))
}
sentence <- "She sells hundreds of sea oysters on the sea shore."
result <- find_largest_words(sentence)
print(result)








