# DONE ead the input
# DONE Isolate each row as a piece of data
# DONE Have to process each row
#   DONE Find the largest value in a row
#   DONE Find the smallest value in a row
#   DONE Find the row difference between big/small in a row
# DONE Sum up all of the row differences
import csv

def process_row(row):
    # convert to ints
    int_list = map(int, row)

    max_value = max(int_list)
    min_value = min(int_list)
    difference = max_value - min_value

    print("min: " + str(min_value) + " max: " + str(max_value) + " difference: " + str(difference))

    return difference

def process_file(filename):
    totals = 0
    # open up the file as a generic file
    with open(filename, 'rb') as csvfile:
        # create a csv reader which means we want python to read as a csv
        csvreader = csv.reader(csvfile)

        # loop through each row in the file
        for row in csvreader:
            # process, then add to our final total
            difference = process_row(row)
            totals = totals + difference

    return totals

part_1_answer = process_file("day2_input.csv")

print("Final answer is: ")
print(part_1_answer)


#======================================================
# Alternative way of solving using map instead of looping
#======================================================

def process_file_alt(filename):
    # open up the file as a generic file
    with open(filename, 'rb') as csvfile:
        # create a csv reader which means we want python to read as a csv
        csvreader = csv.reader(csvfile)

        # map the process_row function over each row 
        # and get a list of differences back
        processed = map(process_row, csvreader)

        # sum up all of the differences
        return sum(processed)

part_1_answer = process_file_alt("day2_input.csv")
print("Final answer_alt is: ")
print(part_1_answer)