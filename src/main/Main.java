package main;

import java.io.IOException;
import java.util.Timer;
import java.util.TimerTask;

import process.QueryProcessing;
import process.StreamGobbler;
import query.SelectiveQuery;

import codeGenerator.CodeGenerator;

public class Main {

	public static void main(String[] args) throws IOException, InterruptedException {
//		String example = "QUERY (\\x -> if creditLimit x > 500 then cons (customerName x, cityName (located x locations)) else nil)\r\n"
//				+ "FROM customers\r\n" + "TO algebraic graph";

//		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
//		selectiveQuery.printParseTree();
//		System.out.println();
//		System.out.println(selectiveQuery.getHaskellCode());

		QueryProcessing queryProcess = new QueryProcessing();
		StreamGobbler outputGobbler = queryProcess.getStreamGobbler();

		// The timer demonstrates a query that arrives to the system 20sec after it is started.
		Timer timer = new Timer();
		timer.schedule(new TimerTask() {
			@Override
			public void run() {
				System.out.println("Runned! See the outputLog file for the result.");
				outputGobbler.executeQuery("foldr (\\x xs -> x:xs) [] orders");
			}
		}, 20 * 1000);

	}

}
