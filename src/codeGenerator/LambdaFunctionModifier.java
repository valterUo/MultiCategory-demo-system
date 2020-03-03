package codeGenerator;

import java.util.ArrayList;

import org.json.JSONArray;
import org.json.JSONObject;

import collectionMetaData.DecodeMetaData;
import query.LambdaFunction;
import query.QueryBlock;

public class LambdaFunctionModifier {
	private JSONObject metadata;

	public LambdaFunctionModifier() {
		DecodeMetaData decodedmetadata = new DecodeMetaData();
		this.metadata = decodedmetadata.getCollectionMapping();
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
			for (int i = 0; i < sizeOfSmallerCollection; i++) {
				JSONObject targetConsFunction = (JSONObject) targetConsFunctions.get(i);
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

	public ArrayList<QueryBlock> queryBlockModifier(ArrayList<QueryBlock> blocks) {
		ArrayList<QueryBlock> translatedBlocks = new ArrayList<QueryBlock>();
		for (QueryBlock query : blocks) {
			translatedBlocks.add(queryBlockLambdaFunctionModifier(query));
		}
		return translatedBlocks;
	}

}
