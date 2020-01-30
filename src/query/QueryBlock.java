package query;

import java.util.ArrayList;

import scanner.SelectiveQueryScanner;

public class QueryBlock {
	private String mainQuery;
	private ArrayList<LambdaFunction> lambdaFunctions;
	private String sourceCollectionName;
	private String sourceCollectionModel;
	private String targetModel;

	public QueryBlock(String mainQuery) {
		this.mainQuery = mainQuery;
		this.lambdaFunctions = parseLambdaFunctions(mainQuery);
		this.sourceCollectionName = parseOtherInformation(mainQuery, "FROM");
		this.sourceCollectionModel = parseOtherInformation(mainQuery, "AS");
		this.targetModel = parseOtherInformation(mainQuery, "TO");
	}

	private ArrayList<LambdaFunction> parseLambdaFunctions(String query) {
		SelectiveQueryScanner scanner = new SelectiveQueryScanner();
		ArrayList<LambdaFunction> lambdaFunctions = new ArrayList<LambdaFunction>();
		ArrayList<String> lambdaFunctionStrings = scanner.scanLambdaFunctionsFromQueryBlock(query);
		for (String element : lambdaFunctionStrings) {
			if(element.startsWith("\\")) {
				lambdaFunctions.add(new LambdaFunction(element));
			}
		}
		return lambdaFunctions;
	}
	
	private String parseOtherInformation(String query, String keyword) {
		String[] keywords = query.split(" ");
		for(int i = 0; i < keywords.length; i++) {
			if(keywords[i].equals(keyword)) {
				return keywords[i+ 1].trim(); 
			}
		}
		return null;
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
		String result = "QUERY element: " + this.mainQuery 
				+ "\n Source collection name: " + this.sourceCollectionName 
				+ "\n Source collection model: " + this.sourceCollectionModel 
				+ "\n Target model: " + this.targetModel + "\n";
		for(LambdaFunction lambda : this.lambdaFunctions) {
			result += "  Lambda function: " + lambda.toString() + "\n";
		}
		return result;
	}

}
