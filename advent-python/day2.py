# DONE ead the input
# DONE Isolate each row as a piece of data
# DONE Have to process each row
#   DONE Find the largest value in a row
#   DONE Find the smallest value in a row
#   DONE Find the row difference between big/small in a row
# DONE Sum up all of the row differences
import csv

def process_row(row):
    max_value = max(row)
    min_value = min(row)
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
            # convert each item in the row to an int
            int_row = map(int, row)

            # process, then add to our final total
            difference = process_row(int_row)
            totals = totals + difference

    return totals

part_1_answer = process_file("day2_input.csv")

print("Final answer is: ")
print(part_1_answer)