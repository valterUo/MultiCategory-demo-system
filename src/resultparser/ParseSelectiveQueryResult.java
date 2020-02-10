package resultparser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;

public class ParseSelectiveQueryResult {
	
	public static String parseResult(File outputlog, File storeParsedResult) throws FileNotFoundException, IOException {
		String result = parseResultFromFile(outputlog);
		storeResult(result, storeParsedResult);
		initializeResultFile(outputlog);
		return result;
	}
	
	public static String parseResultFromFile(File file) throws FileNotFoundException, IOException {
		String result = "";
		String lines = "";
		try (BufferedReader br = new BufferedReader(new FileReader(file))) {
		    String line;
		    while ((line = br.readLine()) != null) {
		       lines += line;
		    }
		}
		String pattern = "(?<=XMLParser>)|(?=\\*Main)";
		String[] splittedFile = lines.split(pattern);
		for(int i = 0; i < splittedFile.length; i++) {
			if(splittedFile[i].contains("*Main")) {
				result += splittedFile[i + 1];
			}
		}
		if(result.contains("Leaving GHCi.")) {
			throw new UnexpectedHaskellTerminationException(result);
		}
		if(result.contains("error") && result.contains("<interactive>")) {
			throw new HaskellExecutionFailureExeception(result);
		}
		return result;
	}
	
	public static void storeResult(String result, File file) {
		try {
			PrintWriter writer = new PrintWriter(file);
			writer.print(result + "\\n");
			writer.close();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public static void initializeResultFile(File file) {
		try {
			PrintWriter writer = new PrintWriter(file);
			writer.print("*Main CSVParser D3jsAlgebraicGraphParser GraphFunctions HelsinkiMultiModelRepo.Film.DataParser "
					+ "HelsinkiMultiModelRepo.Film.DataState HelsinkiMultiModelRepo.Film.SchemaCategory "
					+ "HelsinkiMultiModelRepo.Patent.DataParser HelsinkiMultiModelRepo.Patent.DataState "
					+ "HelsinkiMultiModelRepo.Patent.SchemaCategory HelsinkiMultiModelRepo.Person.DataParser "
					+ "HelsinkiMultiModelRepo.Person.DataState HelsinkiMultiModelRepo.Person.SchemaCategory HelsinkiMultiModelRepo.University.DataParser "
					+ "HelsinkiMultiModelRepo.University.DataState HelsinkiMultiModelRepo.University.SchemaCategory NimbleGraph.NimbleGraph "
					+ "NimbleGraph.NimbleGraphToD3js RdfFunctions SimpleDemo.DataParser SimpleDemo.DataState SimpleDemo.SchemaCategory "
					+ "Unibench.DataParser Unibench.DataState Unibench.SchemaCategory XMLParser> ");
			writer.close();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}	
	}

}
