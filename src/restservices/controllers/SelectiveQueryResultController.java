package restservices.controllers;

import java.util.List;

import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

import io.jsondb.JsonDBTemplate;
import jsondb.JsonDB;
import restservices.selectiveQueryService.SelectiveQueryResult;
import restservices.selectiveQueryService.SelectiveQueryResultNotFoundException;

@RestController
class SelectiveQueryResultController {

	private final JsonDBTemplate jsonDBresult;

	public SelectiveQueryResultController() {
		JsonDB jsonDB = new JsonDB("jsondbfiles", "restservices.selectiveQueryService", "SelectiveQueryResultInstances");
		this.jsonDBresult = jsonDB.getTemplate();
	}
	
	@CrossOrigin(origins = "http://localhost:3000")
	@GetMapping("/selectiveQueryResults")
	List<SelectiveQueryResult> all() {
		this.jsonDBresult.reLoadDB();
		List<SelectiveQueryResult> results = this.jsonDBresult.findAll(SelectiveQueryResult.class);
		System.out.println(results);
		if (results.isEmpty()) {
			throw new SelectiveQueryResultNotFoundException("The collection is empty.");
		}
		return results;
	}

	@CrossOrigin(origins = "http://localhost:3000")
	@GetMapping("/selectiveQueryResults/{id}")
	List<SelectiveQueryResult> one(@PathVariable String id) {
		this.jsonDBresult.reLoadDB();
		String jxQuery = String.format("/.[queryId='%s']", id);
		List<SelectiveQueryResult> result = this.jsonDBresult.find(jxQuery, SelectiveQueryResult.class);
		if (result == null) {
			throw new SelectiveQueryResultNotFoundException(id);
		}
		return result;
	}

	@CrossOrigin(origins = "http://localhost:3000")
	@DeleteMapping("/selectiveQueryResults/{id}")
	String deleteSelectiveQueryResult(@PathVariable Long id) {
		this.jsonDBresult.reLoadDB();
		SelectiveQueryResult removed = this.jsonDBresult.findById(id, SelectiveQueryResult.class);
		if (removed == null) {
			return "The object is not in the collection.";
		}
		this.jsonDBresult.remove(removed, SelectiveQueryResult.class);
		return "The object with id " + id + " is removed.";
	}
}