import csv

# This python file contains functions that able user to clean and cut and correct biased data. 
# It is unnecessary to implement additional error handling to the data structures of MultiCategory demo system because of biased data although it is possible.
# Such error handling would just make the main idea to fade out.

# Correct these paths:
# Delimiter , for patents
whole_file_of_patents = "D:\\Helsinki Multi-Model Datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\patent.table"
# Delimiter , for citations
whole_file_of_citations = "D:\\Helsinki Multi-Model Datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\citation.graph"
# Delimiter ; for inventors (You need to change delimiter from , to ; if you use original inventors table)
whole_file_of_inventors = "D:\\Helsinki Multi-Model Datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\new_inventor.table"

patents = dict({})

with open(whole_file_of_patents, "r", encoding="utf8") as csv_patent:
    csv_reader_patent = csv.reader(csv_patent, delimiter=',')
    next(csv_reader_patent)
    for row in csv_reader_patent:
        patents[row[0]] = row

g = open("shortened_patents.table", "w", encoding="utf8")
h = open("shortened_citation.graph", "w", encoding="utf8")
found_patents = []
i = 0
with open(whole_file_of_citations, "r", encoding="utf8") as csv_file:
    csv_reader = csv.reader(csv_file, delimiter=',')
    next(csv_reader)
    for row in csv_reader:
        if i <= 5000:
            if row[0] not in found_patents and row[1] not in found_patents:
                if row[0] in patents.keys() and row[1] in patents.keys():
                    g.write(";".join(patents[row[0]]) + "\n")
                    g.write(";".join(patents[row[1]]) + "\n")
                    h.write(";".join(row) + "\n")
                    found_patents.append(row[0])
                    found_patents.append(row[1])
                    i = i + 1
            elif row[0] not in found_patents:
                if row[0] in patents.keys() and row[1] in patents.keys():
                    g.write(";".join(patents[row[0]]) + "\n")
                    h.write(";".join(row) + "\n")
                found_patents.append(row[0])
                i = i + 1
            elif row[1] not in found_patents:
                if row[0] in patents.keys() and row[1] in patents.keys():
                    g.write(";".join(patents[row[1]]) + "\n")
                    h.write(";".join(row) + "\n")
                found_patents.append(row[1])
                i = i + 1
            else:
                if row[0] in patents.keys() and row[1] in patents.keys():
                    h.write(";".join(row) + "\n")
                i = i + 1
        else:
            break

g.close()
h.close()

selected_patents = dict({})

with open("shortened_patents.table", "r", encoding="utf8") as csv_patent:
    csv_reader_patent = csv.reader(csv_patent, delimiter=';')
    for row in csv_reader_patent:
        selected_patents[row[0]] = row

j = open("shortened_inventors.table", "w", encoding="utf8")

with open(whole_file_of_inventors, "r", encoding="utf8") as csv_inventors:
    csv_reader = csv.reader(csv_inventors, delimiter=';')
    next(csv_reader)
    for row in csv_reader:
        if row[0] in selected_patents.keys():
            j.write(";".join(row) + "\n")

j.close()