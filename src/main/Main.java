package main;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;

import org.springframework.boot.autoconfigure.SpringBootApplication;

import jsondb.JsonDB;
import restservices.ServiceApplication;

@SpringBootApplication
public class Main {

	public static void main(String[] args) throws IOException, InterruptedException {
		initializeAppFiles();
		ServiceApplication.main(args);
	}

	public static void initializeAppFiles() throws IOException {
		emptyHaskellProgramOutputFile();
		emptyJSONdbDatabases();
	}

	public static void emptyHaskellProgramOutputFile() throws IOException {
		File file = new File("output//output");
		if (file.createNewFile()) {
			System.out.println("File created: " + file.getName());
		} else {
			System.out.println("File " + file.getName() + " already exists.");
		}
		PrintWriter writer;
		try {
			writer = new PrintWriter(file);
			writer.print("");
			writer.close();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		System.out.println("Haskell output file initialized. The results to queries will be parsed from the file \""
				+ file.getName() + "\".");
	}

	public static void emptyJSONdbDatabases() {
		JsonDB jsonDBquery = new JsonDB("jsondbfiles", "restservices.executeQueryService", "ExecutedQueryInstances");
		JsonDB jsonDBresult = new JsonDB("jsondbfiles", "restservices.selectiveQueryService",
				"SelectiveQueryResultInstances");
		jsonDBquery.emptyDB();
		jsonDBresult.emptyDB();
	}

}
