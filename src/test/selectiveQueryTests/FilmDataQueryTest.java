package test.selectiveQueryTests;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import query.SelectiveQuery;

class FilmDataQueryTest {

	@Test
	void testQuery1() {
		String example = "QUERY (\\x xs -> if length xs < 20 then cons x xs else xs)\nFROM films\nTO json";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldr (\\x xs -> if length xs < 20 then x : xs else xs ) [] films";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(), selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "Film query test 1");
	}
	
	@Test
	void testQuery2() {
		String example = "QUERY (\\x xs -> if isInfixOf \"Howard Hall\" (actors x) then cons x xs else xs)\nFROM films\nTO json";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldr (\\x xs -> if isInfixOf \"Howard Hall\" ( actors x ) then x : xs else xs ) [] films";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(), selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "Film query test 2");
	}
	
	@Test
	void testQuery3() {
		String example = "QUERY (\\x xs -> if HelsinkiMultiModelRepo.Film.SchemaCategory.title x == \"Romeo and Juliet\" then cons x xs else xs)\nFROM films\nTO json";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "foldr (\\x xs -> if HelsinkiMultiModelRepo.Film.SchemaCategory.title x == \"Romeo and Juliet\" then x : xs else xs ) [] films";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(), selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "Film query test 3");
	}
	
	@Test
	void testQuery4() {
		String example = "QUERY (\\x newrdf -> if length (triplesOf newrdf) < 20 then cons newrdf x else newrdf)\nFROM filmGraph\nTO rdf";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "(foldrdf (\\x newrdf -> if length ( triplesOf newrdf ) < 20 then addTriple newrdf x else newrdf ) RDF.empty filmGraph) :: RDF TList";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(), selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "Film query test 4");
	}

}
