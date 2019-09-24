import csv

#f = open("C:\\Users\\Valter Uotila\\Desktop\\Unibench datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\patent.table", "r")
#g = open("C:\\Users\\Valter Uotila\\Desktop\\Unibench datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\new_patent_semicolon.table", "w")

#h = open("C:\\Users\\Valter Uotila\\Desktop\\Unibench datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\citation.graph", "r")
#j = open("C:\\Users\\Valter Uotila\\Desktop\\Unibench datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\new_citation.graph", "w")
k = open("C:\\Users\\Valter Uotila\\Desktop\\Unibench datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\new_inventor_piece.table", "w")

# for line in f.readlines():
#      new_line = line.replace(',', ';').replace("\n", "")
#      g.write(new_line + "\n")

# Choose first 1000 patents among all the > 2 000 000 patents
# i = 0
# for line in f.readlines()[784610:]:
#     if i < 1000:
#         new_line = line.replace(',', ';').replace("\n", "")
#         g.write(new_line + "\n")
#         i+=1
#     else:
#         break

# Parse 1000 lines so that we get patents' ids that refer to the graph data
# lines = h.readlines()
# with open("C:\\Users\\Valter Uotila\\Desktop\\Unibench datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\new_patent_semicolon.table") as csv_file:
#     csv_reader = csv.reader(csv_file, delimiter=';')
#     for row in csv_reader:
#         #Choose all the relationships from citation.graph that contain some of the 10 000 chosen patents
#         i = 0
#         for line in lines:
#             if row[0] in line:
#                 print(row[0])
#                 new_line = line.replace(',', ';').replace("\n", "")
#                 j.write(new_line + "\n")
#             i = i + 1
#         print(i)

# Parse inventors so that we choose only those inventors that have some patent among 1000 patents
def inventorfunction():
    with open("C:\\Users\\Valter Uotila\\Desktop\\Unibench datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\new_inventor.table") as inventor_csv_file:
        csv_reader_inventor = csv.reader(inventor_csv_file, delimiter=';')
        for inventor in csv_reader_inventor:
            with open("C:\\Users\\Valter Uotila\\Desktop\\Unibench datasets\\Patent_dataset\\Patent_dataset\\orignal_dataset\\new_patent_semicolon.table") as patent_csv_file:
                csv_reader_patent = csv.reader(patent_csv_file, delimiter = ';')
                for patent in csv_reader_patent:
                    #print(inventor[0])
                    #print(patent[0])
                    if inventor[0] in patent[0] or patent[0] in inventor[0]:
                        print(patent[0])
                        k.write(';'.join(inventor) + "\n")
                        if patent[0] == "3859242":
                            return
        

#f.close()
#g.close()
#h.close()
#j.close()
inventorfunction()
k.close()