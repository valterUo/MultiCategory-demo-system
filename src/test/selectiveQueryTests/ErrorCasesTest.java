package test.selectiveQueryTests;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import query.SelectiveQuery;

class ErrorCasesTest {

	@Test
	void testQuery8() {
		String example = "LET t BE\nQUERY (\\x xs -> if ((sum $ map productPrice (orderProducts x)) > 100) then x:xs else xs)\nFROM orders\nTO xml\nIN\nQUERY (\\x xs -> (orderNumber x, customerName(ordered x customers), countryName(located(ordered x customers) locations)):xs)\nFROM t\nTO relational";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
		String answer = "";
		assertEquals(answer.replaceAll("\\s{1,}", " ").trim(),
				selectiveQuery.getHaskellCode().replaceAll("\\s{1,}", " ").trim(), "Error case: ");
	}

}
