# DONE read the input
# DONE Isolate each row as a piece of data
# DONE Have to process each row
#   DONE Find the largest value in a row
#   DONE Find the smallest value in a row
#   DONE Find the row difference between big/small in a row
# DONE Sum up all of the row differences

# Part 2
# DONE Divide every number by every other number in the row

import csv

def process_row(row):
    # convert to ints
    int_list = map(int, row)

    max_value = max(int_list)
    min_value = min(int_list)
    difference = max_value - min_value

    print("min: " + str(min_value) + " max: " + str(max_value) + " difference: " + str(difference))

    return difference

def process_row_divisible(row):
    int_list = map(int, row)
    length = len(int_list)
    total = 0

    i = 0
    while i < len(int_list):
        first_digit = int_list[i]
        j = 0
        while j < len(int_list):
            if j != i:
                other_digit = int_list[j]
                division = float(first_digit) / other_digit
                is_digit = division.is_integer()
                if is_digit:
                    total += division
            j += 1
        
        i += 1

    return total


def process_file(filename, row_function):
    totals = 0
    # open up the file as a generic file
    with open(filename, 'rb') as csvfile:
        # create a csv reader which means we want python to read as a csv
        csvreader = csv.reader(csvfile)

        # loop through each row in the file
        for row in csvreader:
            # process, then add to our final total
            difference = row_function(row)
            totals = totals + difference

    return totals

part_1_answer = process_file("day2_input.csv", process_row)
part_2_answer = process_file("day2_input.csv", process_row_divisible)

print("Final answer is: ")
print(part_1_answer)

print("Final answer Part 2 is: ")
print(part_2_answer)


#======================================================
# Alternative way of solving using map instead of looping
#======================================================

def process_file_alt(filename, row_function):
    # open up the file as a generic file
    with open(filename, 'rb') as csvfile:
        # create a csv reader which means we want python to read as a csv
        csvreader = csv.reader(csvfile)

        # map the process_row function over each row 
        # and get a list of differences back
        processed = map(row_function, csvreader)

        # sum up all of the differences
        return sum(processed)

def check_division(x, y):
    # check division 
    division = float(x) / y
    if division.is_integer():
        return division
    
    return None

def get_divisible_division(numbers):
    # compare first number to every other number
    number = numbers[0]
    other_numbers = numbers[1:]
    divisions = map(lambda x: check_division(number, x), other_numbers)

    # filter out anything that came back 'None'
    filtered = filter(lambda x: x is not None, divisions)
    if filtered:
        return filtered[0]

    # didn't find anything, call this with the other numbers
    return get_divisible_division(other_numbers)

def process_row_divisible_alt(row):
    int_list = map(int, row)
    # do a reverse sort so that division works properly
    ints_sorted = list(reversed(sorted(int_list)))
    divisibles = get_divisible_division(ints_sorted)
    return divisibles

part_1_answer = process_file_alt("day2_input.csv", process_row)
print("Final answer_alt is: ")
print(part_1_answer)

part_2_answer = process_file_alt("day2_input.csv", process_row_divisible_alt)
print("Final answer_alt2 is: ")
print(part_2_answer)
