package test;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import query.SelectiveQuery;

class SelectiveQueryTest {

	@Test
	void testQuery1() {
		String example = "LET t BE\r\n" + " QUERY (\\x -> if customerName x == \"Alice\" then cons x else nil)\r\n"
				+ " FROM customers\r\n" + " TO relational\r\n" + " IN\r\n"
				+ " QUERY (\\x -> if any (\\y -> knows x y customers) t then cons x else nil)\r\n"
				+ " FROM customers\r\n" + " TO algebraic graph";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "let t = foldg  [] (\\x -> if customerName x == \"Alice\" then [x]  else [] ) (\\x y -> union x y ) (\\x y -> union x y )  customers in foldg  Algebra.Graph.empty (\\x -> if any ( \\y -> knows x y customers ) t then Vertex x else Algebra.Graph.empty ) (\\x y -> overlay x y ) (\\x y -> connect x y )  customers";
		assertEquals(answer, selectiveQuery.getHaskellCode(), "Query test 1");
	}

	@Test
	void testQuery2() {
		String example = "LET t BE QUERY (\\v g -> case (vertexValue v) of Right(person) -> g; Left(post) -> if isInfixOf \"tennis\" (content post) then addVertex v g else g) "
				+ "FROM personCreatedPostGraph " + "TO nimblegraph " + "IN "
				+ "QUERY (\\v g -> nimbleGraphUnion (outGoingNeighbors v personCreatedPostGraph) g) " + "FROM t "
				+ "AS nimblegraph " + "TO nimblegraph";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "let t = foldNimble (\\v g -> case ( vertexValue v ) of Right ( person ) -> g; Left ( post ) -> if isInfixOf \"tennis\" ( content post ) then addVertex v g else g ) (\\edge newGraph -> case ( Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.source edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) ) of Nothing -> newGraph; Just ( sourceVertex ) -> case Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.target edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) of Nothing -> newGraph; Just ( targetVertex ) -> addEdge edge newGraph )  emptyNimbleGraph personCreatedPostGraph in foldNimble (\\v g -> nimbleGraphUnion ( outGoingNeighbors v personCreatedPostGraph ) g ) (\\edge newGraph -> case ( Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.source edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) ) of Nothing -> newGraph; Just ( sourceVertex ) -> case Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.target edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) of Nothing -> newGraph; Just ( targetVertex ) -> addEdge edge newGraph )  emptyNimbleGraph t";
		assertEquals(answer, selectiveQuery.getHaskellCode(), "Query test 2");
	}
	
	@Test
	void testQuery3() {
		String example = "QUERY (\\x xs -> cons x xs)\r\n" + 
				" FROM locations\r\n" + 
				" TO relational";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldr (\\x xs -> x : xs )  [] locations";
		assertEquals(answer, selectiveQuery.getHaskellCode(), "Query test 3");
	}
	
	
	@Test
	void testQuery4() {
		String example = "QUERY (\\x xs -> if xs == [] then (\"size\", 1):xs else let (a,b) = (xs !! 0) in (a, b + 1):[])\r\n" + 
				"FROM locations\r\n" + 
				"TO relational";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldr (\\x xs -> if xs == [] then ( \"size\", 1 ) :xs else let ( a,b ) = ( xs !! 0 ) in ( a, b + 1 ) :[] )  [] locations";
		assertEquals(answer, selectiveQuery.getHaskellCode(), "Query test 4");
	}
	
	@Test
	void testQuery5() {
		String example = "QUERY (\\x xs -> cons x xs)\r\n" + 
				" FROM orders\r\n" + 
				" TO xml";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldr (\\x xs -> x : xs )  [] orders";
		assertEquals(answer, selectiveQuery.getHaskellCode(), "Query test 5");
	}
	
	@Test
	void testQuery6() {
		String example = "LET t BE\r\n" + 
				"QUERY (\\x xs -> if orderNumber x == \"3qqqeq9\" then (orderProducts x) ++ xs else xs)\r\n" + 
				"FROM orders\r\n" + 
				"TO relational\r\n" + 
				"IN\r\n" + 
				"QUERY (\\x xs -> if productPrice x > 50 then cons x xs else xs)\r\n" + 
				"FROM t\r\n" + 
				"AS relational\r\n" + 
				"TO relational";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "let t = foldr (\\x xs -> if orderNumber x == \"3qqqeq9\" then ( orderProducts x ) ++ xs else xs )  [] orders in foldr (\\x xs -> if productPrice x > 50 then x : xs else xs )  [] t";
		assertEquals(answer, selectiveQuery.getHaskellCode(), "Query test 6");
	}
	
	@Test
	void testQuery7() {
		String example = "QUERY (\\x -> if creditLimit x > 3000 then cons x else nil)\r\n" + 
				"FROM customers\r\n" + 
				"TO algebraic graph";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldg  Algebra.Graph.empty (\\x -> if creditLimit x > 3000 then Vertex x else Algebra.Graph.empty ) (\\x y -> overlay x y ) (\\x y -> connect x y )  customers";
		assertEquals(answer, selectiveQuery.getHaskellCode(), "Query test 7");
	}
	
	@Test
	void testQuery8() {
		String example = "LET t BE\r\n" + 
				"QUERY (\\x -> if customerId x == 6 then cons x else nil)\r\n" + 
				"FROM customers\r\n" + 
				"TO relational\r\n" + 
				"IN\r\n" + 
				"LET k BE\r\n" + 
				"QUERY (\\x -> if any (\\y -> knows x y customers) t then cons x else nil)\r\n" + 
				"FROM customers\r\n" + 
				"TO relational\r\n" + 
				"IN\r\n" + 
				"QUERY (\\x xs -> if creditLimit x > 1000 then cons x xs else xs)\r\n" + 
				"FROM k\r\n" + 
				"AS relational\r\n" + 
				"TO relational";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "let t = foldg  [] (\\x -> if customerId x == 6 then [x]  else [] ) (\\x y -> union x y ) (\\x y -> union x y )  customers in let k = foldg  [] (\\x -> if any ( \\y -> knows x y customers ) t then [x]  else [] ) (\\x y -> union x y ) (\\x y -> union x y )  customers in foldr (\\x xs -> if creditLimit x > 1000 then x : xs else xs )  [] k";
		assertEquals(answer, selectiveQuery.getHaskellCode(), "Query test 8");
	}
	
	@Test
	void testQuery9() {
		String example = "LET t BE\r\n" + 
				"QUERY (\\x -> if (customerId x ==  3 || customerId x == 4) then cons x else nil)\r\n" + 
				"FROM customers\r\n" + 
				"TO algebraic graph\r\n" + 
				"IN\r\n" + 
				"QUERY (\\x -> findTargetNeighbors x customers)\r\n" + 
				"FROM t\r\n" + 
				"AS algebraic graph\r\n" + 
				"TO algebraic graph";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "let t = foldg  Algebra.Graph.empty (\\x -> if ( customerId x == 3 || customerId x == 4 ) then Vertex x else Algebra.Graph.empty ) (\\x y -> overlay x y ) (\\x y -> connect x y )  customers in foldg  Algebra.Graph.empty (\\x -> findTargetNeighbors x customers ) (\\x y -> overlay x y ) (\\x y -> connect x y )  t";
		assertEquals(answer, selectiveQuery.getHaskellCode(), "Query test 9");
	}
	
	@Test
	void testQuery10() {
		String example = "QUERY (\\x -> if creditLimit x > 500 then cons (customerName x, cityName (located x locations)) else nil)\r\n" + 
				"FROM customers\r\n" + 
				"TO algebraic graph";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldg  Algebra.Graph.empty (\\x -> if creditLimit x > 500 then Vertex ( customerName x, cityName ( located x locations ) ) else Algebra.Graph.empty ) (\\x y -> overlay x y ) (\\x y -> connect x y )  customers";
		assertEquals(answer, selectiveQuery.getHaskellCode(), "Query test 10");
	}

}
