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
		String example = "QUERY (\\x -> if creditLimit x > 500 then cons (customerName x, cityName (located x locations)) else nil)\r\n" + 
				"FROM customers\r\n" + 
				"TO algebraic graph";

		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
//		selectiveQuery.printParseTree();
//		System.out.println();
		System.out.println(selectiveQuery.getHaskellCode());

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
