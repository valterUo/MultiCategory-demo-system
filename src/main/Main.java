package main;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import collectionMetaData.DecodeMetaData;
import query.QueryBlock;
import query.SelectiveQuery;
import scanner.SelectiveQueryScanner;
import org.json.JSONArray;
import org.json.JSONObject;

import codeGenerator.CodeGenerator;

public class Main {

	public static void main(String[] args) throws IOException, InterruptedException {
		String example = " QUERY (\\x -> if any (\\y -> knows x y customers) t then cons x else nil) (\\x xs -> if any (\\y -> knows x y customers) t then cons x xs else nil) (\\x xs -> if any (\\y -> knows x y customers) t then cons x xs else nil)\r\n" + 
				" FROM customers\r\n" + 
				" TO graph";
		String example2 = "LET t BE QUERY (\\v g -> case (vertexValue v) of Right(person) -> g; Left(post) -> if isInfixOf \"tennis\" (content post) then addVertex v g else g) "
				+ "FROM personCreatedPostGraph "
				+ "TO nimblegraph "
				+ "IN "
				+ "QUERY (\\v g -> nimbleGraphUnion (outGoingNeighbors v personCreatedPostGraph) g) "
				+ "FROM t "
				+ "AS nimblegraph "
				+ "TO nimblegraph";
		String example3 = "LET t BE\r\n" + 
				" QUERY (\\x -> if customerName x == \"Alice\" then cons x else nil)\r\n" + 
				" FROM customers\r\n" + 
				" TO relational\r\n" + 
				" IN\r\n" + 
				" QUERY (\\x -> if any (\\y -> knows x y customers) t then cons x else nil)\r\n" + 
				" FROM customers\r\n" + 
				" TO algebraic graph";
		SelectiveQuery selectiveQuery = new SelectiveQuery(example3);
		CodeGenerator gen = new CodeGenerator();
		gen.selectiveQueryModifier(selectiveQuery);
		//selectiveQuery.printParseTree();
		for(QueryBlock query : selectiveQuery.getQueryBlocks()) {
			//System.out.println(query);
			System.out.println(gen.generateFoldFunctionFromQueryBlock(query));
		}
		
		
		
//		Runtime r = Runtime.getRuntime();
//	    Process p = r.exec("ghci");
//	            p.waitFor();
//	    OutputStream output = p.getOutputStream();
//	    ByteArrayOutputStream byte1 = new ByteArrayOutputStream();
//	    output.write("let x = 5\r\n".getBytes());
//	    output.write("x".getBytes());
//		output.write(byte1.toByteArray());
//	    String result=byte1.toString();
//	    System.out.println(result);
//	    p.destroy();  

	}

}
