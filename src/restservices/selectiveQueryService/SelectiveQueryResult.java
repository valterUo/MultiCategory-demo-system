package restservices.selectiveQueryService;

import java.util.UUID;

import io.jsondb.annotation.Document;
import io.jsondb.annotation.Id;

@Document(collection = "SelectiveQueryResultInstances", schemaVersion = "1.0")
public class SelectiveQueryResult {
	@Id
	private String id;
	private String queryId;
	private String result;
	private String model;
	private String parsedQuery;

	SelectiveQueryResult() {
	}

	public SelectiveQueryResult(String queryId, String result, String parsedQuery, String model) {
		this.id = UUID.randomUUID().toString();
		this.result = result;
		this.model = model;
		this.parsedQuery = parsedQuery;
		this.queryId = queryId;
	}

	public String getId() {
		return this.id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getResult() {
		return this.result;
	}

	public void setResult(String result) {
		this.result = result;
	}

	public String getModel() {
		return this.model;
	}

	public void setModel(String model) {
		this.model = model;
	}

	public String getParsedQuery() {
		return this.parsedQuery;
	}

	public void setParsedQuery(String parsedQuery) {
		this.parsedQuery = parsedQuery;
	}

	public String getQueryId() {
		return queryId;
	}

	public void setQueryId(String queryId) {
		this.queryId = queryId;
	}

}