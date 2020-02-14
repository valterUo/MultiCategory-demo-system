package restservices.executeQueryService;

import java.util.UUID;

import io.jsondb.annotation.Id;

import io.jsondb.annotation.Document;
import query.SelectiveQuery;

@Document(collection = "ExecutedQueryInstances", schemaVersion = "1.0")
public class ExecutedQuery {

	@Id
	private String id;
	private String originalQuery;
	private String parsedQuery;
	private String model;

	public ExecutedQuery() {
	}

	public ExecutedQuery(String originalQuery) {
		this.id = UUID.randomUUID().toString();
		this.originalQuery = originalQuery;
		SelectiveQuery selectiveQuery = new SelectiveQuery(originalQuery);
		this.parsedQuery = selectiveQuery.getHaskellCode();
		this.model = selectiveQuery.getTargetModel();
	}

	public String getModel() {
		return model;
	}

	public String getId() {
		return this.id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getOriginalQuery() {
		return this.originalQuery;
	}

	public void setOriginalQuery(String originalQuery) {
		this.originalQuery = originalQuery;
	}

	public String getParsedQuery() {
		return this.parsedQuery;
	}

	public void setParsedQuery(String parsedQuery) {
		this.parsedQuery = parsedQuery;
	}

}
