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
	private JSONObject datasetDefs;

	public CodeGenerator() {
		DecodeMetaData decodedmetadata = new DecodeMetaData();
		this.metadata = decodedmetadata.getCollectionMapping();
		this.datasetDefs = decodedmetadata.getDataSetDefinitions();
	}

	public String generateFoldFunctionFromQueryBlock(QueryBlock query) {
		String fold = "";
		try {
			String sourceCollectionName = query.getSourceCollectionName();
			String sourceCollectionModel = this.datasetDefs.getString(sourceCollectionName);
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
				System.out.println("No collection model match!");
			}
		} catch (Exception e) {
			System.out.println("Error! Source collection model was not found!");
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
			int sizeOfSmallerCollection;
			if (functions.size() > targetConsFunctions.length()) {
				System.out.println(
						"Warning! There are more lambda functions in the query than the collection construction functions in the datatype's definition. Redundant functions are ignored.");
				sizeOfSmallerCollection = targetConsFunctions.length();
			} else if (functions.size() < targetConsFunctions.length()) {
				System.out.println(
						"Warning! There are less lambda functions in the query than the collection construction functions in the datatype's definition. The compiler applies default settings for non defined functions.");
				sizeOfSmallerCollection = functions.size();
			} else {
				sizeOfSmallerCollection = functions.size();
			}
			for (int i = 0; i < sizeOfSmallerCollection; i++) {
				JSONObject targetConsFunction = (JSONObject) targetConsFunctions.get(i);
				JSONObject sourceConsFunction = (JSONObject) sourceConsFunctions.get(i);
				functions.get(i).modifyConsInLambdaFunction(targetConsFunction.getString("name"),
						sourceConsFunction.getInt("amountOfParameters"), targetConsFunction.getBoolean("operator"));
				functions.get(i).modifyLambdaFunction("nil", targetInitialCollection);
			}
		} catch (Exception e) {
			System.out.println("The data model is not in the definitions.");
		}
		return query;
	}
}
