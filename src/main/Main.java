package main;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.lang.ProcessBuilder.Redirect;
import java.util.Scanner;
import java.util.concurrent.Executors;

import collectionMetaData.DecodeMetaData;
import process.ResultConsumer;
import process.StreamGobbler;
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

//		SelectiveQuery selectiveQuery = new SelectiveQuery(example);
//		selectiveQuery.printParseTree();
//		System.out.println();
//		System.out.println(selectiveQuery.getHaskellCode());

//		Runtime r = Runtime.getRuntime();
//	    Process p = r.exec("ghci");
//	    p.waitFor();
//	    OutputStream output = p.getOutputStream();
//	    ByteArrayOutputStream byte1 = new ByteArrayOutputStream();
//	    output.write("let x = 5\r\n".getBytes());
//	    output.write("x".getBytes());
//		output.write(byte1.toByteArray());
//	    String result=byte1.toString();
//	    System.out.println(result);
//	    p.destroy();  
		
		boolean isWindows = System.getProperty("os.name")
				  .toLowerCase().startsWith("windows");
		
//		ResultConsumer consumer = new ResultConsumer();
//		
//		//String homeDirectory = System.getProperty("user.home");
//		//System.out.println(homeDirectory);
		String dir = "C:\\Users\\Valter Uotila\\Desktop\\demo-system-backend-Haskell\\MultiCategory";
//		Process process;
//		if (isWindows) {
//		    process = Runtime.getRuntime()
//		      .exec(String.format("cmd.exe /c dir %s", dir));
//		} else {
//		    process = Runtime.getRuntime()
//		      .exec(String.format("sh -c ls %s", dir));
//		}
//		StreamGobbler streamGobbler = 
//		  new StreamGobbler(process.getInputStream(), System.out::println);
//		Executors.newSingleThreadExecutor().submit(streamGobbler);
//		int exitCode = process.waitFor();
//		assert exitCode == 0;
		
		Scanner scan = new Scanner(System.in);
		
		ProcessBuilder builder = new ProcessBuilder();//.inheritIO();
		//builder.redirectErrorStream(true);
		File log = new File("outputLog");
		//ResultConsumer consumer = new ResultConsumer();
		builder.redirectOutput(Redirect.appendTo(log));
		builder.redirectInput(Redirect.INHERIT);
		if (isWindows) {
		    builder.command("cmd.exe", "/c", "stack ghci");
		} else {
		    builder.command("sh", "-c", "ls");
		}
		builder.directory(new File(dir));
		Process process = builder.start();
		//process.waitFor();
//		OutputStream output = process.getOutputStream();
//		InputStream input = process.getInputStream ();
//		BufferedReader reader = new BufferedReader (new InputStreamReader(input));
//		BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(output));
		
//		writer.write("foldr (\\x xs -> x:xs) [] locations\r\n");
//		writer.flush();
//		writer.write("t\r\n");
//		writer.flush();
		
//		StreamGobbler streamGobbler = 
//				  new StreamGobbler(process.getInputStream(), System.out::println);
//				Executors.newSingleThreadExecutor().submit(streamGobbler);
//				
//		execute("foldr (\\x xs -> x:xs) [] locations", writer, process);
//		execute("foldr (\\x xs -> x:xs) [] locations", writer, process);
//		execute("foldr (\\x xs -> x:xs) [] locations", writer, process);
		
		
//		while (scan.hasNext()) {
//		    String command = scan.nextLine();
//		    if (command.trim().equals("quit")) {
//		        // Putting 'exit' amongst the echo --EOF--s below doesn't work.
//		        writer.write(":quit\n");
//		    } else {
//		        writer.write("((" + command + ") && echo --EOF--) || echo --EOF--\n");
//		    }
//		    writer.flush();
//
//		    String line = reader.readLine();
//		    System.out.println(line);
//		    while (line != null && ! line.trim().contains("*Main")) {
//		        System.out.println ("Stdout: " + line);
//		        line = reader.readLine();
//		    }
//		    if (line == null) {
//		        break;
//		    }
//		}

//		String command = "";
//		while(!command.equals("exit")) {
//			System.out.print("Enter a command: ");
//			command = scan.nextLine();
//			writer.write(command);
//			writer.flush();
//			String result = reader.readLine();
//			System.out.println(result);
//		}
		//writer.close();
		//process.destroy();
		int exitCode = process.waitFor();
		assert exitCode == 0;

	}
	
	public static void execute(String command, BufferedWriter writer, Process process) {
		try {
			writer.write(command);
			System.out.println(command);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		try {
			writer.flush();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		StreamGobbler streamGobbler = 
				  new StreamGobbler(process.getInputStream(), System.out::println);
		Executors.newSingleThreadExecutor().submit(streamGobbler);
	}
}
