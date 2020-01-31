package collectionMetaData;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
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
