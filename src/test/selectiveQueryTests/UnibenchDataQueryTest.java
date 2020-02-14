package test.selectiveQueryTests;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import query.SelectiveQuery;

class UnibenchDataQueryTest {

	@Test
	void testQuery1() {
		String example = "QUERY (\\x xs -> if length xs > 20 then xs else if (personBrowserUsed x == \"Safari\" && gender x == \"female\" && lastName x == \"Li\") then cons x xs else xs) FROM persons TO relational";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldr (\\x xs -> if length xs > 20 then xs else if ( personBrowserUsed x == \"Safari\" && gender x == \"female\" && lastName x == \"Li\" ) then x : xs else xs ) [] persons";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(),
				selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "Unibench data query test 1");
	}

	@Test
	void testQuery2() {
		String example = "QUERY (\\x xs -> if vendorCountry x == \"Wales\" then cons x xs else xs) FROM vendors TO relational";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldr (\\x xs -> if vendorCountry x == \"Wales\" then x : xs else xs )  [] vendors";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(),
				selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "Unibench data query test 2");
	}

	@Test
	void testQuery3() {
		String example = "QUERY (\\x xs -> if xs == nil then cons (\"size\", 1) xs else let (a,b) = (xs !! 0) in cons (a, b + 1) nil) FROM persons TO relational";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldr (\\x xs -> if xs == [] then (\"size\", 1 ) : xs else let ( a,b ) = ( xs !! 0 ) in (a, b + 1 ) : [] ) [] persons";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(),
				selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "Unibench data query test 3");
	}

	@Test
	void testQuery4() {
		String example = "QUERY (\\x xs -> if totalprice(order x ) > 2100.00 then cons (order x) xs else xs)\nFROM invoices\nTO xml";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldr (\\x xs -> if totalprice ( order x ) > 2100.00 then (order x ) : xs else xs ) [] invoices";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(),
				selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "Unibench data query test 4");
	}

	@Test
	void testQuery4mod() {
		String example = "QUERY (\\x xs -> if totalprice(order x ) > 2100.00 then cons (order x) (xs) else xs)\nFROM invoices\nTO xml";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldr (\\x xs -> if totalprice ( order x ) > 2100.00 then (order x ) :(xs ) else xs ) [] invoices";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(),
				selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "Unibench data query test 4 mod");
	}

	@Test
	void testQuery5() {
		String example = "QUERY (\\vertex newGraph -> let person = vertexValue vertex in if firstName person == \"Li\" && lastName person == \"Li\" then cons vertex newGraph else newGraph)\nFROM personKnowsPersonGraph\nTO nimblegraph";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldNimble (\\vertex newGraph -> let person = vertexValue vertex in if firstName person == \"Li\" && lastName person == \"Li\" then addVertex vertex newGraph else newGraph ) (\\edge newGraph -> case ( Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.source edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) ) of Nothing -> newGraph; Just ( sourceVertex ) -> case Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.target edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) of Nothing -> newGraph; Just ( targetVertex ) -> addEdge edge newGraph ) emptyNimbleGraph personKnowsPersonGraph";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(),
				selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "Unibench data query test 5");
	}

	@Test
	void testQuery6() {
		String example = "LET t BE\nQUERY (\\vertex newGraph -> let person = vertexValue vertex in if personsId person == 26388279076888 then cons vertex newGraph else newGraph)\nFROM personKnowsPersonGraph\nTO nimblegraph\nIN\nQUERY (\\vertex newGraph -> inComingNeighbors vertex personKnowsPersonGraph)\nFROM t\nAS nimblegraph\nTO nimblegraph";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "let t = foldNimble (\\vertex newGraph -> let person = vertexValue vertex in if personsId person == 26388279076888 then addVertex vertex newGraph else newGraph ) (\\edge newGraph -> case ( Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.source edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) ) of Nothing -> newGraph; Just ( sourceVertex ) -> case Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.target edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) of Nothing -> newGraph; Just ( targetVertex ) -> addEdge edge newGraph ) emptyNimbleGraph personKnowsPersonGraph in foldNimble (\\vertex newGraph -> inComingNeighbors vertex personKnowsPersonGraph ) (\\edge newGraph -> case ( Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.source edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) ) of Nothing -> newGraph; Just ( sourceVertex ) -> case Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.target edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) of Nothing -> newGraph; Just ( targetVertex ) -> addEdge edge newGraph ) emptyNimbleGraph t";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(),
				selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "Unibench data query test 6");
	}

	@Test
	void testQuery7() {
		String example = "QUERY (\\vertex newGraph -> case (vertexValue vertex) of Right(person) -> if firstName person == \"Ning\" then cons vertex newGraph else newGraph; Left(post) -> if postId post == 1649267452025 then cons vertex newGraph else newGraph)\nFROM personCreatedPostGraph\nTO nimblegraph";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldNimble (\\vertex newGraph -> case ( vertexValue vertex ) of Right ( person ) -> if firstName person == \"Ning\" then addVertex vertex newGraph else newGraph; Left ( post ) -> if postId post == 1649267452025 then addVertex vertex newGraph else newGraph ) (\\edge newGraph -> case ( Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.source edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) ) of Nothing -> newGraph; Just ( sourceVertex ) -> case Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.target edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) of Nothing -> newGraph; Just ( targetVertex ) -> addEdge edge newGraph ) emptyNimbleGraph personCreatedPostGraph";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(),
				selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "Unibench data query test 7");
	}

	@Test
	void testQuery8() {
		String example = "LET t BE\nQUERY (\\v g -> case (vertexValue v) of Right(person) -> g; Left(post) -> if isInfixOf \"tennis\" (content post) then cons v g else g)\nFROM personCreatedPostGraph\nTO nimblegraph\nIN\nQUERY (\\v g -> nimbleGraphUnion (outGoingNeighbors v personCreatedPostGraph) g)\nFROM t\nAS nimblegraph\nTO nimblegraph";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "let t = foldNimble (\\v g -> case ( vertexValue v ) of Right ( person ) -> g; Left ( post ) -> if isInfixOf \"tennis\" ( content post ) then addVertex v g else g ) (\\edge newGraph -> case ( Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.source edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) ) of Nothing -> newGraph; Just ( sourceVertex ) -> case Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.target edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) of Nothing -> newGraph; Just ( targetVertex ) -> addEdge edge newGraph ) emptyNimbleGraph personCreatedPostGraph in foldNimble (\\v g -> nimbleGraphUnion ( outGoingNeighbors v personCreatedPostGraph ) g ) (\\edge newGraph -> case ( Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.source edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) ) of Nothing -> newGraph; Just ( sourceVertex ) -> case Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.target edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) of Nothing -> newGraph; Just ( targetVertex ) -> addEdge edge newGraph ) emptyNimbleGraph t";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(),
				selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "Unibench data query test 8");
	}

	@Test
	void testQuery9() {
		String example = "QUERY (\\vertex newGraph -> case (vertexValue vertex) of Left(person) -> newGraph; Right(product) -> if unibenchProductId product == 5604 then nimbleGraphUnion (inComingNeighbors vertex personToProductGraph) newGraph else newGraph)\nFROM personToProductGraph\nTO nimblegraph";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldNimble (\\vertex newGraph -> case ( vertexValue vertex ) of Left ( person ) -> newGraph; Right ( product ) -> if unibenchProductId product == 5604 then nimbleGraphUnion ( inComingNeighbors vertex personToProductGraph ) newGraph else newGraph ) (\\edge newGraph -> case ( Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.source edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) ) of Nothing -> newGraph; Just ( sourceVertex ) -> case Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.target edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) of Nothing -> newGraph; Just ( targetVertex ) -> addEdge edge newGraph ) emptyNimbleGraph personToProductGraph";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(),
				selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "Unibench data query test 9");
	}

	@Test
	void testQuery10() {
		String example = "QUERY (\\vertex newGraph -> case (vertexValue vertex) of Left(post) -> if postLength post > (Just 1000) then cons vertex newGraph else newGraph; Right(product) -> newGraph)\nFROM postHasProductGraph\nTO nimblegraph";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldNimble (\\vertex newGraph -> case ( vertexValue vertex ) of Left ( post ) -> if postLength post > ( Just 1000 ) then addVertex vertex newGraph else newGraph; Right ( product ) -> newGraph ) (\\edge newGraph -> case ( Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.source edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) ) of Nothing -> newGraph; Just ( sourceVertex ) -> case Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.target edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) of Nothing -> newGraph; Just ( targetVertex ) -> addEdge edge newGraph ) emptyNimbleGraph postHasProductGraph";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(),
				selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "Unibench data query test 10");
	}

	@Test
	void testQuery11() {
		String example = "LET t BE\nQUERY (\\v g -> case (vertexValue v) of Left(post) -> g; Right(product) -> if unibenchProductPrice product > (Just 500.0) then cons v g else g)\nFROM postHasProductGraph\nTO nimblegraph\nIN\nQUERY (\\v g -> nimbleGraphUnion g (inComingNeighbors v postHasProductGraph))\nFROM t\nAS nimblegraph\nTO nimblegraph";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "let t = foldNimble (\\v g -> case ( vertexValue v ) of Left ( post ) -> g; Right ( product ) -> if unibenchProductPrice product > ( Just 500.0 ) then addVertex v g else g ) (\\edge newGraph -> case ( Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.source edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) ) of Nothing -> newGraph; Just ( sourceVertex ) -> case Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.target edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) of Nothing -> newGraph; Just ( targetVertex ) -> addEdge edge newGraph ) emptyNimbleGraph postHasProductGraph in foldNimble (\\v g -> nimbleGraphUnion g ( inComingNeighbors v postHasProductGraph ) ) (\\edge newGraph -> case ( Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.source edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) ) of Nothing -> newGraph; Just ( sourceVertex ) -> case Map.lookup ( vertexId $ NimbleGraph.NimbleGraph.target edge ) ( NimbleGraph.NimbleGraph.vertices newGraph ) of Nothing -> newGraph; Just ( targetVertex ) -> addEdge edge newGraph ) emptyNimbleGraph t";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(),
				selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "Unibench data query test 11");
	}

	@Test
	void testQuery12() {
		String example = "QUERY (\\x xs -> if totalprice(x) > 3500.00 then cons x xs else xs)\nFROM unibenchOrders\nTO json";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldr (\\x xs -> if totalprice ( x ) > 3500.00 then x : xs else xs ) [] unibenchOrders";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(),
				selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "Unibench data query test 12");
	}

	@Test
	void testQuery13() {
		String example = "QUERY (\\x xs -> if length xs > 0 then if unibenchProductPrice(head xs) < unibenchProductPrice x then cons x nil else xs else cons x nil)\nFROM unibenchProducts\nTO relational";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldr (\\x xs -> if length xs > 0 then if unibenchProductPrice ( head xs ) < unibenchProductPrice x then x : [] else xs else x : [] ) [] unibenchProducts";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(),
				selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "Unibench data query test 13");
	}

	@Test
	void testQuery14() {
		String example = "LET t BE\nQUERY (\\x xs -> if orderid x == \"aeafd60c-8e60-4daf-9136-d3714911bde7\" then cons x xs else xs)\nFROM unibenchOrders\nTO relational\nIN\nQUERY (\\x xs -> cons (orderedBy x persons) xs)\nFROM t\nAS relational\nTO relational";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "let t = foldr (\\x xs -> if orderid x == \"aeafd60c-8e60-4daf-9136-d3714911bde7\" then x : xs else xs ) [] unibenchOrders in foldr (\\x xs -> (orderedBy x persons ) : xs ) [] t";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(),
				selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "Unibench data query test 14");
	}

	@Test
	void testQuery15() {
		String example = "QUERY (\\x xs -> if totalprice(x) > 3500.00 then Connect (cons (orderid(x), orderdate(x))) xs else xs)\nFROM unibenchOrders\nTO algebraic graph";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldr (\\x xs -> if totalprice ( x ) > 3500.00 then Connect ( Vertex ( orderid ( x ) , orderdate ( x ) ) ) xs else xs ) Algebra.Graph.empty unibenchOrders";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(),
				selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "Unibench data query test 15");
	}

	@Test
	void testQuery16() {
		String example = "QUERY (\\x xs -> if length xs > 40 then xs else orderline(order x) ++ xs)\nFROM invoices\nTO relational";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldr (\\x xs -> if length xs > 40 then xs else orderline ( order x ) ++ xs ) [] invoices";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(),
				selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "Unibench data query test 16");
	}

}
