package test.selectiveQueryTests;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import query.SelectiveQuery;

class PersonDataQueryTest {

	@Test
	void testQuery1() {
		String example = "";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "";
		assertEquals(answer, selectiveQuery.getHaskellCode(), "Person query test 1");
	}
	
	@Test
	void testQuery2() {
		String example = "";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "";
		assertEquals(answer, selectiveQuery.getHaskellCode(), "Person query test 2");
	}

}
