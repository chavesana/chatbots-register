#Levenshtein distance (or edit distance) between two strings is the number of deletions, insertions, or substitutions required
#  to transform source string into target string. For example, if source is "book" and target is "back", to transform "book" to
# "back" you will need to change first "o" to "a", second "o" to "c", without additional deletions and insertions, thus, levenshtein 
# distance will be 2.
# Algorithm Implementation/Strings/Levenshtein distance. (2020, April 16). Wikibooks, The Free Textbook Project.
# Retrieved 22:57, May 8, 2020 from https://en.wikibooks.org/w/index.php?title=Algorithm_Implementation/Strings/Levenshtein_distance&oldid=3678144.

def levenshtein(s1, s2):
    if len(s1) < len(s2):
        return levenshtein(s2, s1)

    # len(s1) >= len(s2)
    if len(s2) == 0:
        return len(s1)

    previous_row = range(len(s2) + 1)
    for i, c1 in enumerate(s1):
        current_row = [i + 1]
        for j, c2 in enumerate(s2):
            insertions = previous_row[j + 1] + 1 # j+1 instead of j since previous_row and current_row are one character longer
            deletions = current_row[j] + 1       # than s2
            substitutions = previous_row[j] + (c1 != c2)
            current_row.append(min(insertions, deletions, substitutions))
        previous_row = current_row
    
    return previous_row[-1]