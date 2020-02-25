package scanner;

import java.util.ArrayList;

public class SelectiveQueryScanner {

	public ArrayList<String> scanLetBeInBlock(String characters) {
		// Pattern that splits the input string without removing any tokens
		String newLinesToWhiteSpace = "(\\r\\n\\s*)|(\\n\\s*)";
		characters= characters.replaceAll(newLinesToWhiteSpace, " ");
		String pattern = "(?=LET)|(?=BE)|(?=IN)|(?<=LET)|(?<=BE)|(?<=IN)";
		String[] tokens = characters.split(pattern);
		return cleanTokens(tokens);
	}

	public ArrayList<String> scanLambdaFunctionsFromQueryBlock(String characters) { 
		// (\\)\\()(?![^(]*\\)) Match to )( only if after some sequence of character else than ( there is no ).
		String pattern = "(QUERY\\s*\\()|(\\)\\s*FROM)|(\\)\\s*\\()(?![^(]*\\))|(\\)\\()(?![^(]*\\))|(\\)\\s*\\(\\\\)";
		String[] tokens = characters.split(pattern);
		return cleanTokens(tokens);
	}

	public ArrayList<String> scanLambdaFunction(String characters) {
		String pattern = "(QUERY)|(?=FROM)|(?=->)|(\\s)|(?=\\()|(?=\\))|(?<=\\()|(?<=\\))|(?=nil)|(?<=nil)|(?=cons)|(?<=cons)";
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
