package scanner;

import java.util.ArrayList;

public class SelectiveQueryScanner {

	public ArrayList<String> scanLetBeInBlock(String characters) {
		// Pattern that splits the input string without removing any tokens
		String pattern = "(?=LET)|(?=BE)|(?=IN)|(?<=LET)|(?<=BE)|(?<=IN)";
		String[] tokens = characters.split(pattern);
		return cleanTokens(tokens);
	}

	public ArrayList<String> scanLambdaFunctionsFromQueryBlock(String characters) {
		String pattern = "(QUERY\\s\\()|(\\)\\sFROM)|(\\)\\s\\()|(QUERY\\()|(\\)FROM)|(\\)\\()";
		String[] tokens = characters.split(pattern);
		return cleanTokens(tokens);
	}

	public ArrayList<String> scanLambdaFunction(String characters) {
		String pattern = "(QUERY)|(?=FROM)|(?=->)|(\\s)|(?=\\()|(?=\\))|(?<=\\()|(?<=\\))";
		String[] tokens = characters.split(pattern);
		return cleanTokens(tokens);
	}

	public Boolean validateQuery(String query) {
		String pattern = "";
		return query.matches(pattern);
	}

	public ArrayList<String> cleanTokens(String[] tokens) {
		ArrayList<String> cleanedTokens = new ArrayList<>();
		for (String token : tokens) {
			String temp = token.trim().replaceAll("\\r|\\n", "");
			if (!temp.matches("\\s*")) {
				cleanedTokens.add(temp);
			}
		}
		return cleanedTokens;
	}

}
