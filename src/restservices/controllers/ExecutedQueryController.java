package restservices.controllers;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import io.jsondb.JsonDBTemplate;
import jsondb.JsonDB;
import process.ProcessForHaskellProgram;
import process.StreamGobbler;
import restservices.executeQueryService.ExecutedQuery;
import restservices.executeQueryService.ExecutedQueryNotFoundException;
import restservices.selectiveQueryService.SelectiveQueryResult;
import resultparser.ParseSelectiveQueryResult;

@RestController
public class ExecutedQueryController {
	private final StreamGobbler outputGobbler;
	private final JsonDBTemplate jsonDBquery;
	private final JsonDBTemplate jsonDBresult;
	private final ParseSelectiveQueryResult parser;

	public ExecutedQueryController() throws IOException, InterruptedException {
		JsonDB jsonDBQ = new JsonDB("jsondbfiles", "restservices.executeQueryService", "ExecutedQueryInstances");
		JsonDB jsonDBR = new JsonDB("jsondbfiles", "restservices.selectiveQueryService", "SelectiveQueryResultInstances");
		ProcessForHaskellProgram queryProcess = new ProcessForHaskellProgram();
		this.jsonDBquery = jsonDBQ.getTemplate();
		this.jsonDBresult = jsonDBR.getTemplate();
		this.outputGobbler = queryProcess.getStreamGobbler();
		this.parser = new ParseSelectiveQueryResult();
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
	ExecutedQuery newExecutedQuery(@RequestBody ExecutedQuery postExecutedQuery) throws IOException {
		ExecutedQuery newExecutedQuery = new ExecutedQuery(postExecutedQuery.getOriginalQuery());
		this.outputGobbler.executeQuery(newExecutedQuery.getParsedQuery());
		String result = "";
		
		try {
			result = this.parser.parseResult(new File("output//output"), new File("outputStorageFile"));
			SelectiveQueryResult queryResult = new SelectiveQueryResult(newExecutedQuery.getId(), result, newExecutedQuery.getParsedQuery(), newExecutedQuery.getModel());
			this.jsonDBresult.insert(queryResult);
		} catch (IOException e) {
			e.printStackTrace();
			throw e;
		}
		
		String id = newExecutedQuery.getId();
		this.jsonDBquery.insert(newExecutedQuery);
		return this.jsonDBquery.findById(id, ExecutedQuery.class);
	}
}
