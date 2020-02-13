package codeGenerator;

import java.util.ArrayList;

import org.json.JSONArray;
import org.json.JSONObject;

import collectionMetaData.DecodeMetaData;
import query.LambdaFunction;
import query.QueryBlock;
import query.SelectiveQuery;

public class CodeGenerator {
	private JSONObject metadata;

	public CodeGenerator() {
		DecodeMetaData decodedmetadata = new DecodeMetaData();
		this.metadata = decodedmetadata.getCollectionMapping();
	}

	public String generateFoldFunctionFromQueryBlock(QueryBlock query) {
		String fold = "";
		try {
			String sourceCollectionName = query.getSourceCollectionName();
			String sourceCollectionModel = query.getSourceCollectionModel();
			String targetCollectionModel = query.getTargetModel();
			switch (sourceCollectionModel) {
			case "relational":
				fold += "foldr " + query.flattenLambdaFunctions();
				switch (targetCollectionModel) {
				case "relational":
					fold += " [] ";
					break;
				case "algebraic graph":
					fold += " Algebra.Graph.empty ";
					break;
				case "xml":
					fold += " [] ";
					break;
				case "json":
					fold += " [] ";
					break;
				case "rdf":
					fold += " RDF.empty ";
					break;
				case "nimblegraph":
					fold += " emptyNimbleGraph ";
					break;
				default:
					System.out.println("no match");
				}
				fold += sourceCollectionName;
				if (targetCollectionModel.equals("rdf")) {
					fold = "(" + fold + ") :: RDF TList";
				}
				break;
			case "algebraic graph":
				fold += "foldg ";
				switch (targetCollectionModel) {
				case "relational":
					fold += " [] ";
					break;
				case "algebraic graph":
					fold += " Algebra.Graph.empty ";
					break;
				case "xml":
					fold += " [] ";
					break;
				case "json":
					fold += " [] ";
					break;
				case "rdf":
					fold += " RDF.empty ";
					break;
				case "nimblegraph":
					fold += " emptyNimbleGraph ";
					break;
				default:
					System.out.println("no match");
				}
				fold += query.flattenLambdaFunctions() + " " + sourceCollectionName;
				if (targetCollectionModel.equals("rdf")) {
					fold = "(" + fold + ") :: RDF TList";
				}
				break;
			case "xml":
				fold += "foldr " + query.flattenLambdaFunctions();
				switch (targetCollectionModel) {
				case "relational":
					fold += " [] ";
					break;
				case "algebraic graph":
					fold += " Algebra.Graph.empty ";
					break;
				case "xml":
					fold += " [] ";
					break;
				case "json":
					fold += " [] ";
					break;
				case "rdf":
					fold += " RDF.empty ";
					break;
				case "nimblegraph":
					fold += " emptyNimbleGraph ";
					break;
				default:
					System.out.println("no match");
				}
				fold += sourceCollectionName;
				if (targetCollectionModel.equals("rdf")) {
					fold = "(" + fold + ") :: RDF TList";
				}
				break;
			case "json":
				fold += "foldr " + query.flattenLambdaFunctions();
				switch (targetCollectionModel) {
				case "relational":
					fold += " [] ";
					break;
				case "algebraic graph":
					fold += " Algebra.Graph.empty ";
					break;
				case "xml":
					fold += " [] ";
					break;
				case "json":
					fold += " [] ";
					break;
				case "rdf":
					fold += " RDF.empty ";
					break;
				case "nimblegraph":
					fold += " emptyNimbleGraph ";
					break;
				default:
					System.out.println("no match");
				}
				fold += sourceCollectionName;
				if (targetCollectionModel.equals("rdf")) {
					fold = "(" + fold + ") :: RDF TList";
				}
				break;
			case "rdf":
				fold += "(foldrdf " + query.flattenLambdaFunctions();
				switch (targetCollectionModel) {
				case "relational":
					fold += " [] ";
					break;
				case "algebraic graph":
					fold += " Algebra.Graph.empty ";
					break;
				case "xml":
					fold += " [] ";
					break;
				case "json":
					fold += " [] ";
					break;
				case "rdf":
					fold += " RDF.empty ";
					break;
				case "nimblegraph":
					fold += " emptyNimbleGraph ";
					break;
				default:
					System.out.println("no match");
				}
				fold += sourceCollectionName;
				if (targetCollectionModel.equals("rdf")) {
					fold = "(" + fold + ") :: RDF TList";
				}
				break;
			case "nimblegraph":
				fold += "foldNimble " + query.flattenLambdaFunctions();
				switch (targetCollectionModel) {
				case "relational":
					fold += " [] ";
					break;
				case "algebraic graph":
					fold += " Algebra.Graph.empty ";
					break;
				case "xml":
					fold += " [] ";
					break;
				case "json":
					fold += " [] ";
					break;
				case "rdf":
					fold += " RDF.empty ";
					break;
				case "nimblegraph":
					fold += " emptyNimbleGraph ";
					break;
				default:
					System.out.println("no match");
				}
				fold += sourceCollectionName;
				if (targetCollectionModel.equals("rdf")) {
					fold = "(" + fold + ") :: RDF TList";
				}
				break;
			default:
				System.out.println("No source collection model match!");
			}
		} catch (Exception e) {
			System.out.println("Error! Source collection model was not found! Error: " + e);
		}

		return fold;
	}

	public SelectiveQuery selectiveQueryModifier(SelectiveQuery selectiveQuery) {
		for (QueryBlock query : selectiveQuery.getQueryBlocks()) {
			queryBlockLambdaFunctionModifier(query);
		}
		return selectiveQuery;
	}

	public QueryBlock queryBlockLambdaFunctionModifier(QueryBlock query) {
		ArrayList<LambdaFunction> functions = query.getLambdaFunctions();
		JSONObject targetModel = null;
		JSONObject sourceModel = null;
		try {
			targetModel = this.metadata.getJSONObject(query.getTargetModel());
			sourceModel = this.metadata.getJSONObject(query.getSourceCollectionModel());
			JSONArray targetConsFunctions = targetModel.getJSONArray("consFunctions");
			JSONArray sourceConsFunctions = sourceModel.getJSONArray("consFunctions");
			String targetInitialCollection = targetModel.getString("initialCollection");
			int sizeOfSmallerCollection = getSizeOfSmallerCollection(functions, sourceConsFunctions);
			JSONObject targetConsFunction = (JSONObject) targetConsFunctions.get(0);
			for (int i = 0; i < sizeOfSmallerCollection; i++) {
				JSONObject sourceConsFunction = (JSONObject) sourceConsFunctions.get(i);
				functions.get(i).modifyConsInLambdaFunction(targetConsFunction.getString("name"),
						sourceConsFunction.getInt("amountOfParameters"));
				functions.get(i).substituteNil(targetInitialCollection);
			}
		} catch (Exception e) {
			System.out.println("The data model is not in the definitions. Error: " + e);
		}
		return query;
	}

	private int getSizeOfSmallerCollection(ArrayList<LambdaFunction> functions, JSONArray sourceConsFunctions) {
		if (functions.size() > sourceConsFunctions.length()) {
			System.out.println(
					"Warning! There are more lambda functions in the query than the collection construction functions in the datatype's definition. Redundant functions are ignored.");
			return sourceConsFunctions.length();
		} else if (functions.size() < sourceConsFunctions.length()) {
			System.out.println(
					"Warning! There are less lambda functions in the query than the collection construction functions in the datatype's definition. The compiler applies default settings for non defined functions.");
		}
		return functions.size();
	}
}
