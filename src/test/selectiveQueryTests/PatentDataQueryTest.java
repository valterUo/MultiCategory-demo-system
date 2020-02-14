package test.selectiveQueryTests;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import query.SelectiveQuery;

class PatentDataQueryTest {

	@Test
	void testQuery1() {
		String example = "QUERY (\\x xs -> if length xs > 20 then xs else case inventorPatent x patents of Just patent -> cons (lastnam x, firstnam x, patentId(patent)) xs; Nothing -> xs)\nFROM inventors\nTO relational";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldr (\\x xs -> if length xs > 20 then xs else case inventorPatent x patents of Just patent -> (lastnam x, firstnam x, patentId ( patent ) ) : xs; Nothing -> xs ) [] inventors";
		assertEquals(answer, selectiveQuery.getHaskellCode(), "Patent query test 1");
	}
	
	@Test
	void testQuery2() {
		String example = "QUERY (\\x xs -> if xs == nil then cons (\"size of inventors table\", 1) nil else let (a,b) = (xs !! 0) in cons (a, b + 1) nil)\nFROM inventors\nTO relational";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldr (\\x xs -> if xs == [] then (\"size of inventors table\", 1 ) : [] else let ( a,b ) = ( xs !! 0 ) in (a, b + 1 ) : [] ) [] inventors";
		assertEquals(answer, selectiveQuery.getHaskellCode(), "Patent query test 2");
	}
	
	@Test
	void testQuery3() {
		String example = "QUERY (\\x -> if (patentId x == 3858241 || patentId x == 3398406 || patentId x == 3557384 || patentId x == 3634889) then cons ((patentId x, patentCountry x)) else nil)\nFROM patentGraph\nTO algebraic graph";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldg Algebra.Graph.empty (\\x -> if ( patentId x == 3858241 || patentId x == 3398406 || patentId x == 3557384 || patentId x == 3634889 ) then Vertex ( ( patentId x, patentCountry x ) ) else Algebra.Graph.empty ) (\\x y -> overlay x y ) (\\x y -> connect x y ) patentGraph";
		assertEquals(answer, selectiveQuery.getHaskellCode(), "Patent query test 3");
	}
	
	@Test
	void testQuery4() {
		String example = "QUERY (\\x -> case gyear x of Just year -> if year < 1964 then cons((patentId x, patentCountry x)) else nil; Nothing -> cons ((patentId x, patentCountry x)))\nFROM patentGraph\nTO algebraic graph";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldg Algebra.Graph.empty (\\x -> case gyear x of Just year -> if year < 1964 then Vertex ( ( patentId x, patentCountry x ) ) else Algebra.Graph.empty ; Nothing -> Vertex ( ( patentId x, patentCountry x ) ) ) (\\x y -> overlay x y ) (\\x y -> connect x y ) patentGraph";
		assertEquals(answer, selectiveQuery.getHaskellCode(), "Patent query test 4");
	}
	
	@Test
	void testQuery5() {
		String example = "LET t BE\nQUERY (\\x -> if patentId x == 3858249 then cons x else nil)\nFROM patentGraph\nTO relational\nIN\nQUERY (\\x -> if reachable x (head t) patentGraph then cons x else nil)\nFROM patentGraph\nTO algebraic graph";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "let t = foldg [] (\\x -> if patentId x == 3858249 then [x] else [] ) (\\x y -> union x y ) (\\x y -> union x y ) patentGraph in foldg Algebra.Graph.empty (\\x -> if reachable x ( head t ) patentGraph then Vertex x else Algebra.Graph.empty ) (\\x y -> overlay x y ) (\\x y -> connect x y ) patentGraph";
		assertEquals(answer, selectiveQuery.getHaskellCode(), "Patent query test 5");
	}
	
	@Test
	void testQuery6() {
		String example = "QUERY (\\x -> if reachable x (patents ! 3858249) patentGraph then cons x else nil)\nFROM patentGraph\nTO algebraic graph";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldg Algebra.Graph.empty (\\x -> if reachable x ( patents ! 3858249 ) patentGraph then Vertex x else Algebra.Graph.empty ) (\\x y -> overlay x y ) (\\x y -> connect x y ) patentGraph";
		assertEquals(answer, selectiveQuery.getHaskellCode(), "Patent query test 6");
	}
	
	@Test
	void testQuery7() {
		String example = "LET t BE\nQUERY (\\x -> if patentId x ==  3859982 then cons x else nil)\nFROM patentGraph\nTO algebraic graph\nIN\nQUERY (\\x -> findTargetNeighbors x patentGraph)\nFROM t\n AS algebraic graph \nTO algebraic graph";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "let t = foldg Algebra.Graph.empty (\\x -> if patentId x == 3859982 then Vertex x else Algebra.Graph.empty ) (\\x y -> overlay x y ) (\\x y -> connect x y ) patentGraph in foldg Algebra.Graph.empty (\\x -> findTargetNeighbors x patentGraph ) (\\x y -> overlay x y ) (\\x y -> connect x y ) t";
		assertEquals(answer, selectiveQuery.getHaskellCode(), "Patent query test 7");
	}
	
	@Test
	void testQuery8() {
		String example = "QUERY (\\x xs -> if lastnam x == \"Williams\" then Overlay (cons (inventorPatent x patents))  xs else xs)\nFROM inventors\nTO algebraic graph";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldr (\\x xs -> if lastnam x == \"Williams\" then Overlay ( Vertex ( inventorPatent x patents ) ) xs else xs ) Algebra.Graph.empty inventors";
		assertEquals(answer, selectiveQuery.getHaskellCode(), "Patent query test 8");
	}

}
