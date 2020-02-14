package test.selectiveQueryTests;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import query.SelectiveQuery;

class PersonDataQueryTest {

	@Test
	void testQuery1() {
		String example = "QUERY (\\triple total -> if tripleContainsString triple (Just \"Renaud_de_Cormont\") Nothing Nothing then cons total triple else total)\nFROM personGraph\nTO rdf";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "(foldrdf (\\triple total -> if tripleContainsString triple ( Just \"Renaud_de_Cormont\" ) Nothing Nothing then addTriple total triple else total ) RDF.empty personGraph) :: RDF TList";
		assertEquals(answer, selectiveQuery.getHaskellCode(), "Person query test 1");
	}
	
	@Test
	void testQuery2() {
		String example = "QUERY (\\x newrdf -> if length (triplesOf newrdf) < 5 then cons newrdf x else newrdf)\nFROM personGraph\nTO rdf";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "(foldrdf (\\x newrdf -> if length ( triplesOf newrdf ) < 5 then addTriple newrdf x else newrdf ) RDF.empty personGraph) :: RDF TList";
		assertEquals(answer, selectiveQuery.getHaskellCode(), "Person query test 2");
	}

}
