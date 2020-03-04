package collectionMetaData;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

import org.json.JSONArray;
import org.json.JSONObject;

import query.QueryBlock;

public class DecodeMetaData {

	public JSONObject getCollectionMapping() {
		JSONObject obj = new JSONObject(readMetaDataFile("metadata\\CollectionInformation.json"));
		return obj.getJSONObject("collections");
	}
	
	public JSONObject getDataSetDefinitions() {
		JSONObject obj = new JSONObject(readMetaDataFile("metadata\\DataDefinitions.json"));
		return obj.getJSONObject("collections");
	}
	
	public ArrayList<String> getMorphismsForCategoricalView() {
		ArrayList<String> morphismList = new ArrayList<String>();
		JSONObject obj = new JSONObject(readMetaDataFile("metadata\\MorphismsForCategoricalView.json"));
		JSONObject obj4 = obj.getJSONObject("morphisms");
		for(String key : obj4.keySet()) {
			JSONObject obj2 = obj4.getJSONObject(key);
			JSONArray morphisms = obj2.getJSONArray("morphisms");
			for(Object morphism : morphisms) {
				JSONObject obj3 = (JSONObject) morphism;
				morphismList.add(obj3.getString("name"));
			}
		}
		return morphismList;
	}
	
	public String getSourceCollectionModel(QueryBlock query) {
		String sourceCollectionName = query.getSourceCollectionName();
		String sourceCollectionModel = getDataSetDefinitions().getString(sourceCollectionName);
		return sourceCollectionModel;
	}

	public String readMetaDataFile(String filePath) {
		String data = "";
		try {
			File file = new File(filePath);
			Scanner myReader = new Scanner(file);
			while (myReader.hasNextLine()) {
				data += myReader.nextLine();
			}
			myReader.close();
		} catch (FileNotFoundException e) {
			System.out.println("An error occurred.");
			e.printStackTrace();
		}
		return data;
	}

}
