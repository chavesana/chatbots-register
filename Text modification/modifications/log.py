import csv
import constants
import dist

# Creates one log file per style and a CSV file for everything.
# Log per style will contain the list of files changed, along with the features tweaked for this style 
# and a vector of booleans that tells me what features were tweaked per file. To add content to this file, use the save() function
# log.CSV file will contain all the details about the changes.
# This file follows the pattern: style, subdirectory\filename, line, original_sentence, modified_sentence, original_feature, new_feature
# The distance file contains the original and translated sentences, along with the levenshtein score, which says how many changes I did per sentence. I used this file to 
# avoid the redundancies of the lines in the log.csv file.
def create_logfile(style, features, new):
    modifications_log = open(constants.MAIN_DIR + '\\log_' + style + '.txt',"w+")
    modifications_log.write(style + 'Features: ' + ' * '.join(features) + '\n')
    modifications_log.write(style + 'New: ' + ' * '.join(new) + '\n')
    modifications_log.close()
    csv_log = open(constants.MAIN_DIR + '\\log_' + style + '.csv',"w+")
    csv_log.write('Target Style, Filename, Line Number, Original Sentence, Modified Sentence, Feature replaced, Feature attributed\n')
    csv_log.close()
    distance_file = open(constants.MAIN_DIR + '\\' + style + '_distance.csv',"w+")
    distance_file.write('Filename, Line, Original, Translated, Modification score\n')
    distance_file.close()

# Saves the basic output of which features were changed for the listed file
def save(style, filename, replaced):
    modifications_log = open(constants.MAIN_DIR + '\\log_' + style + '.txt',"a+")
    modifications_log.write('File: ' + filename + '\tReplaced feature: ' + ''.join(str(replaced)) + '\n')
    modifications_log.close()

# Creates a CSV files with ALL the lines changed per file and features changed 
def csv_log(folder_filename, style, modified_lines, original, lines, features_to_replace, target_features):
    csv_log = csv.writer(open(constants.MAIN_DIR + '\\log_' + style + '.csv',"a"))
    dist_log = csv.writer(open(constants.MAIN_DIR + '\\' + style + '_distance.csv',"a"))
    for changes in modified_lines:
        line_changed = changes[0]
        feature = changes[1]
        for line_index in line_changed:
            #remove the line break from the end of the lines
            new_original = original[line_index].replace('\n', '')
            new_original = new_original.replace(',', '')
            new_lines = lines[line_index].replace('\n', '')
            new_lines = new_lines.replace(',', '')

            #my_str = '\"' + style + '" , "' + folder_filename + '", "' + str(line_index+1) + '\", \"' + new_original + '\", \"' + new_lines + \
            #    '\", \"' + feature + '\", "' + target_features[features_to_replace.index(feature)] #+ "\"\n"
            #csv_log.writerow([my_str])

            #save the file
            csvdata = [style, folder_filename, str(line_index+1), new_original, new_lines, feature, target_features[features_to_replace.index(feature)]]
            csv_log.writerow(csvdata)

            #calculate the levenshtein distance to know how many changes we made in the line
            dist_score = dist.levenshtein(new_original.lower(), new_lines)
            csvdata = [folder_filename, str(line_index+1), new_original, new_lines, dist_score]
            dist_log.writerow(csvdata)
    #csv_log.close()