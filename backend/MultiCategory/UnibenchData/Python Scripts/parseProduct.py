import csv

new_file = open("new_product.csv", "w")

with open("Product.csv", "r", encoding="utf8") as csv_patent:
    csv_reader_patent = csv.reader(csv_patent, delimiter=',')
    next(csv_reader_patent)
    for row in csv_reader_patent:
        new_row = []
        for element in row:
            element = element.replace("|", "")
            new_row.append(element)
        new_file.write("|".join(new_row) + "\n")

new_file.close()