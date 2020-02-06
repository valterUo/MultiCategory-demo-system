package restservice;

import lombok.Data;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;

import org.json.JSONObject;

@Data
@Entity
class SelectiveQueryResult {

	private @Id @GeneratedValue Long id;
	private String result;
	private String model;
	private String parsedQuery;

	SelectiveQueryResult() {
	}

	SelectiveQueryResult(String result, String model, String parsedQuery) {
		this.result = result;
		this.model = model;
		this.parsedQuery = parsedQuery;
	}

	public Long getId() {
		return this.id;
	}

	public void setId(Long id) {
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

	
}