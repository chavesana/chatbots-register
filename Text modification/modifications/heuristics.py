import os
import re
import constants
import log
import feat

# Replaces all occurrences of the specified feature in the string by the new feature.
def replace_all(guide_utterances, feature, new_feature):
    modified_text = list()
    replaced = list()
    for utter in guide_utterances:
        if feature in utter.lower():
            #find the index of the changed line
            replaced.append(guide_utterances.index(utter))
            # Replace
            utter = utter.lower()
            utter = utter.replace(feature, new_feature)
        # changed or not, add the utterance text to a file in the 'modified' folder
        modified_text.append(utter)
    return replaced, modified_text

def create_modified_files(folder, filename, lines):
    newfile = constants.MAIN_DIR + '\\modified\\' + folder + '\\mod_' + folder + '_' + filename
    with open(newfile, "w+") as modified_file:
        for line in lines:
            modified_file.write(line)
    modified_file.close()

#the script change the sentences in order, and the 'find' searches over the modified sentences

"""MAIN FUNCTION"""
style = 'DailyDialog'

features_to_replace, target_features = feat.create_lists(style)

features = list(zip(features_to_replace, target_features))

log.create_logfile(style, features_to_replace, target_features)
for root, dirs, files in os.walk(constants.DIRECTORY):
    for subdirectory in dirs:
        subdir = constants.DIRECTORY + subdirectory + '\\'
        for filename in os.listdir(subdir):
            folder_filename = subdirectory + '\\' + filename
            # replaced_features is True/False list that tells me if one particular feature was replaced or not.
            # It is the same size as features_to_replace or target_features lists
            replaced_features = list()
            # modified_lines will contain line numbers that represent ALL lines modified in this file
            modified_lines = list()
            with open(subdir + filename, 'U') as f_original:
                lines = f_original.readlines() #build a list with all the lines to be modified
                #Keep the original sentences to save the csv log later
                original = lines

                # Replace each feature in this file
                for feature in features_to_replace:
                    flag_replace = False
                    modified_lines_per_feature, lines = replace_all(lines, feature, target_features[features_to_replace.index(feature)])
                
                    #if this feature were replaced any time, save the lines that were changed
                    if len(modified_lines_per_feature) > 0:
                        flag_replace = True
                        modified_lines.append((modified_lines_per_feature, feature))
                    replaced_features.append(flag_replace)

                if True in replaced_features: #If any feature was found and replaced in this file
                    log.save(style, folder_filename, replaced_features)
                    #create a new file with the new text
                    create_modified_files(subdirectory, filename, lines)
                    log.csv_log(folder_filename, style, modified_lines, original, lines, features_to_replace, target_features)
            f_original.close()