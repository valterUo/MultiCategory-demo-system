package query;

import java.util.ArrayList;

import scanner.SelectiveQueryScanner;

public class LambdaFunction {
	private String mainFunction;
	private ArrayList<String> variables;
	private ArrayList<String> tokens;

	public LambdaFunction(String mainFunction) {
		this.mainFunction = mainFunction;
		this.tokens = parseTokens(mainFunction);
		this.variables = extractVariables(this.tokens);
	}

	private ArrayList<String> extractVariables(ArrayList<String> tokens) {
		ArrayList<String> variables = new ArrayList<String>();
		for (String token : tokens) {
			if (token.trim().equals("->")) {
				break;
			}
			variables.add(token.replace("\\", ""));
		}
		return variables;
	}

	private ArrayList<String> parseTokens(String lambda) {
		SelectiveQueryScanner scanner = new SelectiveQueryScanner();
		ArrayList<String> tokens = scanner.scanLambdaFunction(lambda);
		return tokens;
	}

	public ArrayList<String> getVariables() {
		return this.variables;
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
		String result = this.mainFunction + "\n" + "    Tokens: " + this.tokens.toString();
		return result;
	}

	public void modifyConsInLambdaFunction(String newConsFunction,
			int amountOfParametersInDomainDataModelConsFunction) {
		// System.out.println("New cons function: " + newConsFunction + " and amount of
		// parameters: " + amountOfParametersInDomainDataModelConsFunction);
		switch (newConsFunction) {
		case ":":
			switch (amountOfParametersInDomainDataModelConsFunction) {
			case 1:
				modifyConsInLambdaFunctionOneParameter(newConsFunction);
				break;
			case 2:
				modifyConsInLambdaFunctionTwoParameters(newConsFunction);
				break;
			default:
				System.out.println("Amount of parameters in domain data model's cons function is "
						+ amountOfParametersInDomainDataModelConsFunction + " which is invalid amount.");
				break;
			}
			break;
		default:
			modifyLambdaFunction("cons", newConsFunction);
			break;
		}
	}

	public void modifyConsInLambdaFunctionOneParameter(String newConsFunction) {
		for (int i = 0; i < this.tokens.size(); i++) {
			boolean visitedIf = false;
			if (this.tokens.get(i).trim().equals("cons")
					&& this.tokens.get(i + 1).trim().equals(this.variables.get(0))) {
				this.tokens.set(i, "[" + this.variables.get(0) + "]");
				this.tokens.set(i + 1, "");
				visitedIf = true;
			} else if (i == this.tokens.size() - 1 && visitedIf) {
				System.out.println("Error! The cons function in the lambda function has wrong parameters. Should be "
						+ this.variables.get(0) + ".");
			} else if (this.tokens.get(i).trim().equals("cons") && this.tokens.get(i + 1).trim().equals("(")) {
				modifyConsFunctionFollowedByOneOrTwoParametersClosedInParantheses(i + 1);
			}
		}
	}

	public void modifyConsInLambdaFunctionTwoParameters(String newConsFunction) {
		for (int i = 0; i < this.tokens.size(); i++) {
			boolean visitedIf = false;
			if (this.tokens.get(i).trim().equals("cons") && this.tokens.get(i + 1).trim().equals(this.variables.get(0))
					&& this.tokens.get(i + 2).trim().equals(this.variables.get(1))) {
				this.tokens.set(i, this.variables.get(0));
				this.tokens.set(i + 1, newConsFunction);
				this.tokens.set(i + 2, this.variables.get(1));
				visitedIf = true;
			} else if (this.tokens.get(i).trim().equals("cons") && this.tokens.get(i + 1).trim().equals("(")) {
				modifyConsFunctionFollowedByOneOrTwoParametersClosedInParantheses(i);
			} else if (this.tokens.get(i).trim().equals("cons")) {
				this.tokens.set(i, this.tokens.get(i + 1));
				this.tokens.set(i + 1, newConsFunction);
			} else if (i == this.tokens.size() - 1 && visitedIf) {
				System.out.println("Error! The cons function in the lambda function has wrong parameters. Should be "
						+ this.variables.get(0) + " and " + this.variables.get(1));
			}
		}
	}

	private void modifyConsFunctionFollowedByOneOrTwoParametersClosedInParantheses(int i) {
		Pair<String, Integer> element = findUntilParanthesisClose(i + 1);
		if (element == null) {
			System.out.println("Wrong amount of paranthesis after cons function!");
		}
		this.tokens.set(i, element.fst);
		if (this.tokens.get(element.snd).trim().equals(this.variables.get(1))) {
			this.tokens.set(element.snd, ": " + this.variables.get(1));
		} else if (this.tokens.get(element.snd).trim().equals("(")) {
			Pair<String, Integer> element2 = findUntilParanthesisClose(element.snd);
			if (element2 == null) {
				System.out.println("Wrong amount of paranthesis after cons function!");
			}
			this.tokens.set(element.snd, ":" + element2.fst);
		} else {
			this.tokens.set(element.snd, ": " + this.tokens.get(element.snd));
		}
	}

	private Pair<String, Integer> findUntilParanthesisClose(int i) {
		String result = "(";
		this.tokens.set(i, "");
		Integer paranthesesCount = 0;
		for (int j = i + 1; j < this.tokens.size(); j++) {
			if (this.tokens.get(j).trim().equals(")") && paranthesesCount == 0) {
				this.tokens.set(j, "");
				return new Pair<String, Integer>(result + ")", j + 1);
			} else if (this.tokens.get(j).trim().equals(")") && paranthesesCount > 0) {
				paranthesesCount--;
			} else if (this.tokens.get(j).trim().equals("(")) {
				paranthesesCount++;
			}
			result += this.tokens.get(j).trim() + " ";
			this.tokens.set(j, "");
		}
		return null;
	}

	public void modifyLambdaFunction(String token, String newToken) {
		for (int i = 0; i < this.tokens.size(); i++) {
			if (this.tokens.get(i).trim().equals(token)) {
				this.tokens.set(i, newToken);
			}
		}
	}

	public void substituteNil(String newEmptyCollection) {
		for (int i = 0; i < this.tokens.size(); i++) {
			String token = this.tokens.get(i);
			if (token.contains("nil")) {
				this.tokens.set(i, token.replaceAll("nil", newEmptyCollection));
			}
		}
	}

	public String flattenLambdaFunction() {
		String result = "";
		for (String token : this.tokens) {
			if (!token.equals("")) {
				result += token + " ";
			}
		}
		return result;
	}

}
