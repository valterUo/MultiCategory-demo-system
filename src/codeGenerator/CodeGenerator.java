package codeGenerator;

import java.util.ArrayList;

import org.json.JSONArray;
import org.json.JSONObject;

import collectionMetaData.DecodeMetaData;
import query.LambdaFunction;
import query.QueryBlock;

public class CodeGenerator {
	private JSONObject metadata;
	private JSONObject datasetDefs;

	public CodeGenerator() {
		DecodeMetaData decodedmetadata = new DecodeMetaData();
		this.metadata = decodedmetadata.getCollectionMapping();
		this.datasetDefs = decodedmetadata.getDataSetDefinitions();
	}

	public String generateHaskellCode() {
		String haskellCode = "";
		// Walk first leaves, apply keyword mapping. Then one level up in the tree and
		// construct folds. Last, collect folds in sequences.
		switch ("") {
		case "one":
			System.out.println("one");
			break;
		case "two":
			System.out.println("two");
			break;
		case "three":
			System.out.println("three");
			break;
		default:
			System.out.println("no match");
		}
		return haskellCode;
	}

	public QueryBlock QueryBlockLambdaFunctionModifier(QueryBlock query) {
		ArrayList<LambdaFunction> functions = query.getLambdaFunctions();
		JSONObject dataModel = null;
		try {
			dataModel = this.metadata.getJSONObject(this.datasetDefs.getString(query.getSourceCollectionName()));
			JSONArray consFunctions = dataModel.getJSONArray("consFunctions");
			String initialCollection = dataModel.getString("initialCollection");
			int sizeOfSmallerCollection;
			if (functions.size() > consFunctions.length()) {
				System.out.println(
						"Warning! There are more lambda functions in the query than the collection construction functions in the datatype's definition. Redundant functions are ignored.");
				sizeOfSmallerCollection = consFunctions.length();
			} else if (functions.size() < consFunctions.length()) {
				System.out.println(
						"Warning! There are less lambda functions in the query than the collection construction functions in the datatype's definition. The compiler applies default settings for non defined functions.");
				sizeOfSmallerCollection = functions.size();
			} else {
				sizeOfSmallerCollection = functions.size();
			}
			for (int i = 0; i < sizeOfSmallerCollection; i++) {
				JSONObject consFunction = (JSONObject) consFunctions.get(i);
				functions.get(i).modifyLambdaFunction("cons", consFunction.getString("name"));
				functions.get(i).modifyLambdaFunction("nil", initialCollection);
			}
		} catch (Exception e) {
			System.out.println("The data model is not in the definitions.");
		}
		return query;
	}
}
