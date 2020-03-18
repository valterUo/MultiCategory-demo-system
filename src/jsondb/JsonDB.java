package jsondb;

import io.jsondb.JsonDBTemplate;

public class JsonDB {

	private final String dbFilesLocation;
	private final String baseScanPackage;
	private final JsonDBTemplate template;
	private final String collectionName;

	public JsonDB(String dbFilesLocation, String baseScanPackage, String collectionName) {
		this.collectionName = collectionName;
		this.dbFilesLocation = dbFilesLocation;
		this.baseScanPackage = baseScanPackage;
		this.template = new JsonDBTemplate(this.dbFilesLocation, this.baseScanPackage);
		try {
			this.template.createCollection(this.collectionName);
		} catch (Exception e) {
			System.out.println("The collection " + this.collectionName + " already exists.");
		}
	}

	public void emptyDB() {
		this.template.findAllAndRemove(".", this.collectionName);
	}

	public String getDbFilesLocation() {
		return this.dbFilesLocation;
	}

	public String getBaseScanPackage() {
		return this.baseScanPackage;
	}

	public JsonDBTemplate getTemplate() {
		return this.template;
	}

	public String getEntityClass() {
		return this.collectionName;
	}
}
