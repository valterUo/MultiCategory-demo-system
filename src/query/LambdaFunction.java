package query;

import java.util.ArrayList;

import scanner.SelectiveQueryScanner;

public class LambdaFunction {
	private String mainFunction;
	private ArrayList<String> tokens;
	
	public LambdaFunction(String mainFunction) {
		this.mainFunction = mainFunction;
		this.tokens = parseTokens(mainFunction);
	}

	private ArrayList<String> parseTokens(String lambda) {
		SelectiveQueryScanner scanner = new SelectiveQueryScanner();
		ArrayList<String> tokens = scanner.scanLambdaFunction(lambda);
		return tokens;
	}

	public String getMainFunction() {
		return this.mainFunction;
	}

	public void setMainFunction(String mainFunction) {
		this.mainFunction = mainFunction;
	}

	public ArrayList<String> getTokens() {
		return this.tokens;
	}

	public void setTokens(ArrayList<String> tokens) {
		this.tokens = tokens;
	}
	
	public String toString() {
		String result = this.mainFunction + "\n"
				+ "    Tokens: " + this.tokens.toString();
		return result;
	}
	
	public void modifyLambdaFunction(String token, String newToken) {
		for(int i = 0; i < this.tokens.size(); i++) {
			if(this.tokens.get(i).trim().equals(token)) {
				this.tokens.set(i, newToken);
			}
		}
	}

}
