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

// Complicated function that substitutes right cons function depending on the source and target models.
	public void modifyConsInLambdaFunction(String newConsFunction, int amountOfParameters, boolean operator) {
		if (amountOfParameters != this.variables.size()) {
			System.out.println("Error! Lambda function has wrong number of parameters.");
		} else {
			if (amountOfParameters == 1) {
				switch (newConsFunction) {
				case ":":
					for (int i = 0; i < this.tokens.size(); i++) {
						boolean visitedIf = false;
						if (this.tokens.get(i).trim().equals("cons")
								&& this.tokens.get(i + 1).trim().equals(this.variables.get(0))) {
							this.tokens.set(i, "[" + this.variables.get(0) + "]");
							this.tokens.set(i + 1, "");
							visitedIf = true;
						} else if (i == this.tokens.size() - 1 && visitedIf) {
							System.out.println(
									"Error! The cons function in the lambda function has wrong parameters. Should be "
											+ this.variables.get(0) + ".");
						}
					}
					break;
				default:
					modifyLambdaFunction("cons", newConsFunction);
					break;
				}
			} else if (amountOfParameters == 2) {
				if (!operator) {
					modifyLambdaFunction("cons", newConsFunction);
				} else {
					if (this.variables.size() != 2) {
						System.out.println("Error! Operator cons function requires exactly two variables!");
					} else {
						for (int i = 0; i < this.tokens.size(); i++) {
							boolean visitedIf = false;
							if (this.tokens.get(i).trim().equals("cons")
									&& this.tokens.get(i + 1).trim().equals(this.variables.get(0))
									&& this.tokens.get(i + 2).trim().equals(this.variables.get(1))) {
								this.tokens.set(i, this.variables.get(0));
								this.tokens.set(i + 1, newConsFunction);
								this.tokens.set(i + 2, this.variables.get(1));
								visitedIf = true;
							} else if (i == this.tokens.size() - 1 && visitedIf) {
								System.out.println(
										"Error! The cons function in the lambda function has wrong parameters. Should be "
												+ this.variables.get(0) + " and " + this.variables.get(1));
							}
						}
					}
				}
			} else {
				System.out.println("Error! Too many parameters!");
			}
		}
	}

	public void modifyLambdaFunction(String token, String newToken) {
		for (int i = 0; i < this.tokens.size(); i++) {
			if (this.tokens.get(i).trim().equals(token)) {
				this.tokens.set(i, newToken);
			}
		}
	}

	public String flattenLambdaFunction() {
		String result = "";
		for (String token : this.tokens) {
			result += token + " ";
		}
		return result;
	}

}
