package test.selectiveQueryTests;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import query.SelectiveQuery;

class UniversityDataQueryTest {

	@Test
	void testQuery1() {
		String example = "QUERY (\\x xs -> if length xs > 20 then xs else cons x xs)\nFROM universities\nTO relational";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldr (\\x xs -> if length xs > 20 then xs else x : xs ) [] universities";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(), selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "University query test 1");
	}
	
	@Test
	void testQuery2() {
		String example = "QUERY (\\current total -> if length (triplesOf total) < 20 then cons total current else total)\nFROM famousLocations\nTO rdf";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "(foldrdf (\\current total -> if length ( triplesOf total ) < 20 then addTriple total current else total ) RDF.empty famousLocations) :: RDF TList";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(), selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "University query test 2");
	}
	
	@Test
	void testQuery3() {
		String example = "QUERY (\\x xs -> if length xs > 20 then xs else cons (escapeTriple x) xs)\nFROM famousLocations\nTO relational";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldrdf (\\x xs -> if length xs > 20 then xs else (escapeTriple x ) : xs ) [] famousLocations";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(), selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "University query test 3");
	}
	
	@Test
	void testQuery4() {
		String example = "QUERY (\\x xs -> if isInfixOf \"Football\" (show x) then cons (escapeTriple x) xs else xs)\nFROM famousLocations\nTO relational";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldrdf (\\x xs -> if isInfixOf \"Football\" ( show x ) then (escapeTriple x ) : xs else xs ) [] famousLocations";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(), selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "University query test 4");
	}

}
