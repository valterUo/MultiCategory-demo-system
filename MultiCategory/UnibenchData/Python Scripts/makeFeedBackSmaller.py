import csv

new_file1 = open("small_feedback.csv", "w")
i = 0

with open("Feedback.csv", "r", encoding="utf8") as csv_feedback:
    csv_reader_feedback = csv.reader(csv_feedback, delimiter='|')
    previous_row = next(csv_reader_feedback)
    for row in csv_reader_feedback:
        if previous_row[0] != row[0] or i % 10 == 0:
            new_file1.write("|".join(row) + "\n")
        previous_row = row
        i+=1

new_file1.close()