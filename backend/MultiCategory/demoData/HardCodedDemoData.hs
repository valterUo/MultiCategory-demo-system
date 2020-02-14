module HardCodedDemoData where

-- Graph
customers = edges [(Customer 1 "John" 2000 10, Customer 6 "Mill" 0 11), 
                        (Customer 3 "Alice" 200 12, Customer 6 "Mill" 0 11), 
                        (Customer 6 "Mill" 0 11, Customer 3 "Alice" 200 12), 
                        (Customer 3 "Alice" 200 12, Customer 1 "John" 2000 10), 
                        (Customer 1 "John" 2000 10, Customer 2 "William" 3000 13), 
                        (Customer 0 "Mary" 5000 14, Customer 5 "Erica" 8000 16), 
                        (Customer 4 "William" 30 15, Customer 2 "William" 3000 13), 
                        (Customer 4 "William" 30 15, Customer 5 "Erica" 8000 16), 
                        (Customer 0 "Mary" 5000 14, Customer 0 "Mary" 5000 14), 
                        (Customer 1 "John" 2000 10, Customer 1 "John" 2000 10), 
                        (Customer 2 "William" 3000 13, Customer 2 "William" 3000 13), 
                        (Customer 3 "Alice" 200 12, Customer 3 "Alice" 200 12), 
                        (Customer 4 "William" 30 15, Customer 4 "William" 30 15), 
                        (Customer 5 "Erica" 8000 16, Customer 5 "Erica" 8000 16), 
                        (Customer 6 "Mill" 0 11, Customer 6 "Mill" 0 11), 
                        (Customer 7 "Bob" 9999 10, Customer 7 "Bob" 9999 10)]

-- Relational data:
locations = [Location 10 "Pietari Kalmin katu 5" "Helsinki" 00560 "Finland", 
                Location 11 "Lietaus g. 51" "Vilnius" 04231 "Lithuania", 
                Location 12 "Masterton Castlepoint Road" "Tinui" 5889 "New Zealand", 
                Location 13 "535 Pasir Ris Drive 1" "Northeast" 510535 "Singapore", 
                Location 14 "N 2" "Sandweiler" 5238 "Luxembourg", 
                Location 15 "Avenida Adolfo Eastman" "Olmue" 2330505 "Chile", 
                Location 16 "Industrivej 5" "Kjellerup" 8620 "Denmark"]

--XML data:
products = [Product "2343f" "Toy" 66, 
               Product "3424g" "Book" 40, 
               Product "2543f" "Guitar" 668, 
               Product "1234r" "Carpet" 1, 
               Product "896h" "Jewelry" 5000, 
               Product "5698r" "Car" 9999, 
               Product "7890u" "Cup" 24, 
               Product "5467y" "Pen" 2]

orders = [Order "34e5e79" [Product "2343f" "Toy" 66, Product "3424g" "Book" 40], 
            Order "0cbdf508" [Product"2543f" "Guitar" 668, Product "1234r" "Carpet" 1], 
            Order "4dwtfuu" [Product "2343f" "Toy" 66], 
            Order "3qqqeq9" [Product "2343f" "Toy" 66, Product "3424g" "Book" 40, Product "3424g" "Book" 40, Product "3424g" "Book" 40, Product "2543f" "Guitar" 668], 
            Order "77idy65" [Product "5467y" "Pen" 2, Product "5698r" "Car" 9999], 
            Order "ery63rg" [Product "7890u" "Cup" 24, Product "5467y" "Pen" 2, Product "3424g" "Book" 40, Product "2543f" "Guitar" 668, Product "896h" "Jewelry" 5000, Product "2343f" "Toy" 66]]