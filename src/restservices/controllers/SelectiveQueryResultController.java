package restservices.controllers;

import java.util.List;

import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

import io.jsondb.JsonDBTemplate;
import restservices.selectiveQueryService.SelectiveQueryResult;
import restservices.selectiveQueryService.SelectiveQueryResultNotFoundException;

@RestController
class SelectiveQueryResultController {

	private final JsonDBTemplate jsonDBresult;

	public SelectiveQueryResultController() {
		String dbFilesLocation = "jsondbfiles";
		String baseScanPackage = "restservices.selectiveQueryService";
		this.jsonDBresult = new JsonDBTemplate(dbFilesLocation, baseScanPackage);
		try {
			this.jsonDBresult.createCollection(SelectiveQueryResult.class);
		} catch (Exception e) {
			System.out.println(
					"The collection " + this.jsonDBresult.getCollectionName(SelectiveQueryResult.class) + " already exists.");
		}
	}

	@GetMapping("/selectiveQueryResults")
	List<SelectiveQueryResult> all() {
		List<SelectiveQueryResult> results = this.jsonDBresult.findAll(SelectiveQueryResult.class);
		if (results.isEmpty()) {
			throw new SelectiveQueryResultNotFoundException("The collection is empty.");
		}
		return results;
	}

	// Single item
	@GetMapping("/selectiveQueryResults/{id}")
	SelectiveQueryResult one(@PathVariable String id) {
		SelectiveQueryResult result = this.jsonDBresult.findById(id, SelectiveQueryResult.class);
		if (result == null) {
			// ResponseEntity.notFound().build();
			throw new SelectiveQueryResultNotFoundException(id);
		}
		return result;
	}

	// Delete the result
	@DeleteMapping("/selectiveQueryResults/{id}")
	String deleteSelectiveQueryResult(@PathVariable Long id) {
		SelectiveQueryResult removed = this.jsonDBresult.findById(id, SelectiveQueryResult.class);
		if (removed == null) {
			return "The object is not in the collection.";
		}
		this.jsonDBresult.remove(removed, SelectiveQueryResult.class);
		return "The object with id " + id + " is removed.";
	}
}