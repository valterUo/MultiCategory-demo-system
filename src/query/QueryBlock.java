package query;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import collectionMetaData.DecodeMetaData;
import scanner.SelectiveQueryScanner;

public class QueryBlock {
	private String mainQuery;
	private ArrayList<LambdaFunction> lambdaFunctions;
	private String sourceCollectionName;
	private String sourceCollectionModel;
	private String targetModel;
	private String associatedVariable;

	public QueryBlock(String mainQuery, String associatedVariable) {
		this.mainQuery = mainQuery;
		this.associatedVariable = associatedVariable;
		List<String> endKeywordsForFROM = Arrays.asList("TO", "AS");
		List<String> endKeywordsForAS = Arrays.asList("TO");
		List<String> endKeywordsForTO = Arrays.asList();
		this.sourceCollectionName = parseOtherInformation(mainQuery, "FROM", endKeywordsForFROM);
		if (parseOtherInformation(mainQuery, "AS", endKeywordsForAS) == "") {
			DecodeMetaData metadata = new DecodeMetaData();
			this.sourceCollectionModel = metadata.getSourceCollectionModel(this);
		} else {
			this.sourceCollectionModel = parseOtherInformation(mainQuery, "AS", endKeywordsForAS);
		}
		this.targetModel = parseOtherInformation(mainQuery, "TO", endKeywordsForTO);
		this.lambdaFunctions = parseLambdaFunctions(mainQuery);
	}

	private ArrayList<LambdaFunction> parseLambdaFunctions(String query) {
		SelectiveQueryScanner scanner = new SelectiveQueryScanner();
		ArrayList<LambdaFunction> lambdaFunctions = new ArrayList<LambdaFunction>();
		ArrayList<String> lambdaFunctionStrings = scanner.scanLambdaFunctionsFromQueryBlock(query);
		for (String element : lambdaFunctionStrings) {
			if (element.contains("->")) {
				if (element.startsWith("\\")) {
					lambdaFunctions.add(new LambdaFunction(element));
				} else {
					lambdaFunctions.add(new LambdaFunction("\\" + element));
				}

			}
		}
		lambdaFunctions = validateLambdaFunctions(lambdaFunctions);
		return lambdaFunctions;
	}

	public ArrayList<LambdaFunction> validateLambdaFunctions(ArrayList<LambdaFunction> lambdaFunctions2) {
		switch (this.sourceCollectionModel) {
		case "relational":
			// Requires exatcly one lambda function
			if (lambdaFunctions2.size() < 1) {
				System.out.println("Error! Not enough lambda functions for " + this.sourceCollectionModel);
			}
			break;
		case "algebraic graph":
			if (lambdaFunctions2.size() == 1) {
				switch (this.targetModel) {
				case "relational":
					lambdaFunctions2.add(new LambdaFunction("\\x y -> union x y"));
					lambdaFunctions2.add(new LambdaFunction("\\x y -> union x y"));
					break;
				case "algebraic graph":
					lambdaFunctions2.add(new LambdaFunction("\\x y -> overlay x y"));
					lambdaFunctions2.add(new LambdaFunction("\\x y -> connect x y"));
					break;
				case "xml":
					lambdaFunctions2.add(new LambdaFunction("\\x y -> union x y"));
					lambdaFunctions2.add(new LambdaFunction("\\x y -> union x y"));
					break;
				case "json":
					lambdaFunctions2.add(new LambdaFunction("\\x y -> union x y"));
					lambdaFunctions2.add(new LambdaFunction("\\x y -> union x y"));
					break;
				case "rdf":
					lambdaFunctions2.add(new LambdaFunction("\\x y -> rdfUnion x y"));
					lambdaFunctions2.add(new LambdaFunction("\\x y -> rdfUnion x y"));
					break;
				case "nimblegraph":
					lambdaFunctions2.add(new LambdaFunction("\\x y -> nimbleGraphUnion x y"));
					lambdaFunctions2.add(new LambdaFunction("\\x y -> nimbleGraphUnion x y"));
					break;
				default:
					System.out.println("no match");
				}
			} else if (lambdaFunctions2.size() == 2) {
				switch (this.targetModel) {
				case "relational":
					lambdaFunctions2.add(new LambdaFunction("\\x y -> union x y"));
					break;
				case "algebraic graph":
					lambdaFunctions2.add(new LambdaFunction("\\x y -> connect x y"));
					break;
				case "xml":
					lambdaFunctions2.add(new LambdaFunction("\\x y -> union x y"));
					break;
				case "json":
					lambdaFunctions2.add(new LambdaFunction("\\x y -> union x y"));
					break;
				case "rdf":
					lambdaFunctions2.add(new LambdaFunction("\\x y -> rdfUnion x y"));
					break;
				case "nimblegraph":
					lambdaFunctions2.add(new LambdaFunction("\\x y -> nimbleGraphUnion x y"));
					break;
				default:
					System.out.println("no match");
				}
			} else if (lambdaFunctions2.size() == 3) {
				break;
			} else {
				System.out.println("Error!");
			}
			break;
		case "xml":
			// Requires exatcly one lambda function
			if (lambdaFunctions2.size() < 1) {
				System.out.println("Error! Not enough lambda functions for " + this.sourceCollectionModel);
			}
			break;
		case "json":
			// Requires exatcly one lambda function
			if (lambdaFunctions2.size() < 1) {
				System.out.println("Error! Not enough lambda functions for " + this.sourceCollectionModel);
			}
			break;
		case "rdf":
			// Requires exatcly one lambda function
			if (lambdaFunctions2.size() < 1) {
				System.out.println("Error! Not enough lambda functions for " + this.sourceCollectionModel);
			}
			break;
		case "nimblegraph":
			if (lambdaFunctions2.size() == 1) {
				switch (this.targetModel) {
				case "relational":
					lambdaFunctions2.add(new LambdaFunction("\\x y -> union x y"));
					break;
				case "algebraic graph":
					lambdaFunctions2.add(new LambdaFunction("\\x y -> connect x y"));
					break;
				case "xml":
					lambdaFunctions2.add(new LambdaFunction("\\x y -> union x y"));
					break;
				case "json":
					lambdaFunctions2.add(new LambdaFunction("\\x y -> union x y"));
					break;
				case "rdf":
					lambdaFunctions2.add(new LambdaFunction("\\x y -> rdfUnion x y"));
					break;
				case "nimblegraph":
					lambdaFunctions2.add(new LambdaFunction(
							"\\edge newGraph -> case (Map.lookup (vertexId $ NimbleGraph.NimbleGraph.source edge) (NimbleGraph.NimbleGraph.vertices newGraph)) of Nothing -> newGraph; "
									+ "Just(sourceVertex) -> case Map.lookup (vertexId $ NimbleGraph.NimbleGraph.target edge) (NimbleGraph.NimbleGraph.vertices newGraph) of Nothing -> newGraph; "
									+ "Just(targetVertex) -> addEdge edge newGraph"));
					break;
				default:
					System.out.println("no match");
				}
			} else if (lambdaFunctions2.size() < 0) {
				System.out.println("Error! Not enough lambda functions for " + this.sourceCollectionModel);
			} else {
				break;
			}
			break;
		default:
			System.out.println("no match");
		}
		return lambdaFunctions2;
	}

	private String parseOtherInformation(String query, String startKeyword, List<String> endKeywords) {
		String[] keywords = query.split(" ");
		String result = "";
		for (int i = 0; i < keywords.length; i++) {
			if (keywords[i].equals(startKeyword)) {
				while (i < keywords.length - 1 && !endKeywords.contains(keywords[i + 1].trim())) {
					result += keywords[i + 1] + " ";
					i++;
				}
			}
		}
		return result.trim();
	}

	public String getMainQuery() {
		return this.mainQuery;
	}

	public void setMainQuery(String mainQuery) {
		this.mainQuery = mainQuery;
	}

	public ArrayList<LambdaFunction> getLambdaFunctions() {
		return this.lambdaFunctions;
	}

	public void setLambdaFunctions(ArrayList<LambdaFunction> lambdaFunctions) {
		this.lambdaFunctions = lambdaFunctions;
	}

	public String getSourceCollectionName() {
		return this.sourceCollectionName;
	}

	public String getSourceCollectionModel() {
		return this.sourceCollectionModel;
	}

	public String getTargetModel() {
		return this.targetModel;
	}

	@Override
	public String toString() {
		String result = "QUERY element: " + this.mainQuery + "\n Source collection name: " + this.sourceCollectionName
				+ "\n Source collection model: " + this.sourceCollectionModel + "\n Target model: " + this.targetModel
				+ "\n";
		for (LambdaFunction lambda : this.lambdaFunctions) {
			result += "  Lambda function: " + lambda.toString() + "\n";
		}
		return result;
	}

	public String flattenLambdaFunctions() {
		String result = "";
		for (LambdaFunction lambda : this.lambdaFunctions) {
			result += "(" + lambda.flattenLambdaFunction() + ") ";
		}
		return result;
	}

	public String getAssociatedVariable() {
		return this.associatedVariable;
	}

}
