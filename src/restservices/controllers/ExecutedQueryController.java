package restservices.controllers;

import java.io.IOException;
import java.util.List;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import io.jsondb.JsonDBTemplate;
import process.QueryProcessing;
import process.StreamGobbler;
import restservices.executeQueryService.ExecutedQuery;
import restservices.executeQueryService.ExecutedQueryNotFoundException;
import restservices.selectiveQueryService.SelectiveQueryResult;

@RestController
public class ExecutedQueryController {
	private final StreamGobbler outputGobbler;
	private final JsonDBTemplate jsonDBquery;
	private final JsonDBTemplate jsonDBresult;

	public ExecutedQueryController() throws IOException, InterruptedException {
		String dbFilesLocation = "jsondbfiles";
		String baseScanPackage = "restservices.executeQueryService";
		String baseScanPackage2 = "restservices.selectiveQueryService";
		this.jsonDBquery = new JsonDBTemplate(dbFilesLocation, baseScanPackage);
		this.jsonDBresult = new JsonDBTemplate(dbFilesLocation, baseScanPackage2);
		try {
			this.jsonDBquery.createCollection(ExecutedQuery.class);
		} catch (Exception e) {
			System.out.println(
					"The collection " + this.jsonDBquery.getCollectionName(ExecutedQuery.class) + " already exists.");
		}
		try {
			this.jsonDBresult.createCollection(SelectiveQueryResult.class);
		} catch (Exception e) {
			System.out.println(
					"The collection " + this.jsonDBresult.getCollectionName(ExecutedQuery.class) + " already exists.");
		}
		QueryProcessing queryProcess = new QueryProcessing();
		this.outputGobbler = queryProcess.getStreamGobbler();
	}

	@GetMapping("/executedQueries")
	List<ExecutedQuery> all() {
		List<ExecutedQuery> results = this.jsonDBquery.findAll(ExecutedQuery.class);
		if (results.isEmpty()) {
			throw new ExecutedQueryNotFoundException("The collection is empty.");
		}
		return results;
	}

	@GetMapping("/executedQueries/{id}")
	ExecutedQuery one(@PathVariable String id) {
		ExecutedQuery result = this.jsonDBquery.findById(id, ExecutedQuery.class);
		if (result == null) {
			throw new ExecutedQueryNotFoundException(id);
		}
		return result;
	}

	@PostMapping("/executeQuery")
	ExecutedQuery newExecutedQuery(@RequestBody ExecutedQuery postExecutedQuery) {
		ExecutedQuery newExecutedQuery = new ExecutedQuery(postExecutedQuery.getOriginalQuery());
		this.outputGobbler.executeQuery(newExecutedQuery.getParsedQuery());
		String id = newExecutedQuery.getId();
		this.jsonDBquery.insert(newExecutedQuery);
		return this.jsonDBquery.findById(id, ExecutedQuery.class);
	}

}
