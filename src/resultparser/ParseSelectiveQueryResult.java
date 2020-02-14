package resultparser;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

public class ParseSelectiveQueryResult {

	public String parseResult(File outputlog, File storeParsedResult) throws FileNotFoundException, IOException {
		String result = parseResultFromFile(outputlog);
		storeResult(result, storeParsedResult);
		initializeResultFile(outputlog);
		return result;
	}

	public static String parseResultFromFile(File file) throws FileNotFoundException, IOException {
		String allLines = "";
		String result = null;
		while (result == null) {
			allLines = allLines(new File(file.getPath()));
			if (allLines.endsWith("Leaving GHCi.")) {
				throw new UnexpectedHaskellTerminationException(result);
			}
			result = parseResultFromString(allLines);
		}

		if (result.contains("error") && result.contains("<interactive>")) {
			initializeResultFile(file);
			throw new HaskellExecutionFailureExeception(result);
		}
		return result;
	}

	public static String parseResultFromString(String s) {
		String result = null;
		String pattern = "(?=\\*Main)|(?<=XMLParser>)|(?<=\\*Main)|(?=XMLParser>)";
		String[] splittedLines = s.split(pattern);
		for (int i = 0; i < splittedLines.length; i++) {
			if (i < splittedLines.length - 1 && i > 0) {
				if (splittedLines[i + 1].equals("*Main") && splittedLines[i - 1].equals("XMLParser>")) {
					result = splittedLines[i];
				}
			}
		}
		return result;
	}

	public static String allLines(File file) throws FileNotFoundException, IOException {
		String lines = "";
		try (BufferedReader br = new BufferedReader(new FileReader(file))) {
			String line;
			while ((line = br.readLine()) != null) {
				lines += line;
			}
		}
		return lines;
	}

	public static void storeResult(String result, File file) {
		try (FileWriter fw = new FileWriter(file, true);
				BufferedWriter bw = new BufferedWriter(fw);
				PrintWriter writer = new PrintWriter(bw)) {
			writer.println(result);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public static void initializeResultFile(File file) {
		try {
			PrintWriter writer = new PrintWriter(file);
			writer.write(
					"*Main XMLParser> ");
			writer.close();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
