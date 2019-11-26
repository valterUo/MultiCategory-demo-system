# Unibench schema includes Tag table that contains only numbers from 0 to ~9700. 
# This python file modifies files so that we do not need this Tag table.
# You need to correct the paths.

import csv

new_file1 = open("post_to_product.csv", "w")
new_file2 = open("person_to_product.csv", "w")
tag_to_product_list = []

with open("Dataset\\Product\\tagToProduct.csv", "r", encoding="utf8") as csv_tag_to_product:
    csv_reader_tag_to_product = csv.reader(csv_tag_to_product, delimiter=';')
    for row in csv_reader_tag_to_product:
        tag_to_product_list.append(row[1])

with open("post_hasTag_tag_0_0.csv", "r", encoding="utf8") as csv_post_has_tag:
    csv_reader_post_has_tag = csv.reader(csv_post_has_tag, delimiter='|')
    firstrow = next(csv_reader_post_has_tag)
    new_file1.write(firstrow[0] + "|asin\n")
    for post_tag_row in csv_reader_post_has_tag:
        new_row = []
        new_row.append(post_tag_row[0])
        new_row.append(tag_to_product_list[int(post_tag_row[1])])
        new_file1.write("|".join(new_row) + "\n")

new_file1.close()

with open("person_hasInterest_tag_0_0.csv", "r", encoding="utf8") as csv_person_hasInterest_tag:
    csv_reader_person_hasInterest_tag = csv.reader(csv_person_hasInterest_tag, delimiter='|')
    firstrow = next(csv_reader_person_hasInterest_tag)
    new_file2.write(firstrow[0] + "|asin\n")
    for person_hasInterest_tag_row in csv_reader_person_hasInterest_tag:
        new_row = []
        new_row.append(person_hasInterest_tag_row[0])
        new_row.append(tag_to_product_list[int(person_hasInterest_tag_row[1])])
        new_file2.write("|".join(new_row) + "\n")

new_file2.close()