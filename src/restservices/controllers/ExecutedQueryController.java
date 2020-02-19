package restservices.controllers;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import io.jsondb.JsonDBTemplate;
import jsondb.JsonDB;
import process.ProcessForHaskellProgram;
import process.StreamGobbler;
import restservices.executeQueryService.ExectutionFailedErrorException;
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
	
	@CrossOrigin(origins = "http://localhost:3000")
	@GetMapping("/executedQueries")
	List<ExecutedQuery> all() {
		List<ExecutedQuery> results = this.jsonDBquery.findAll(ExecutedQuery.class);
		if (results.isEmpty()) {
			throw new ExecutedQueryNotFoundException("The collection is empty.");
		}
		return results;
	}

	@CrossOrigin(origins = "http://localhost:3000")
	@GetMapping("/executedQueries/{id}")
	ExecutedQuery one(@PathVariable String id) {
		ExecutedQuery result = this.jsonDBquery.findById(id, ExecutedQuery.class);
		if (result == null) {
			throw new ExecutedQueryNotFoundException(id);
		}
		return result;
	}
	
	@CrossOrigin(origins = "http://localhost:3000")
	@PostMapping("/executeQuery")
	ExecutedQuery newExecutedQuery(@RequestBody ExecutedQuery postExecutedQuery) throws IOException {
		ExecutedQuery newExecutedQuery = new ExecutedQuery(postExecutedQuery.getOriginalQuery());
		String query = newExecutedQuery.getParsedQuery();
		switch(newExecutedQuery.getModel()) {
			case "relational":
				query = "wrapListToJSON $ " + query;
				break;
			case "xml":
				query = "wrapListToJSON $ " + query;
				break;
			case "json":
				query = "wrapListToJSON $ " + query;
				break;
			case "algebraic graph":
				query = "encode $ createD3Graph $ " + query;
				break;
			case "rdf":
				query = "encode $ rdfTriplesToD3Graph $ triplesOf " + query;
				break;
			case "nimblegraph":
				query = "encode $ createD3NimbleGraph $ " + query;
				break;
		}
		this.outputGobbler.executeQuery(query);
		String result = "";
		
		try {
			result = this.parser.parseResult(new File("output//output"), new File("outputStorageFile"));
			SelectiveQueryResult queryResult = new SelectiveQueryResult(newExecutedQuery.getId(), result, newExecutedQuery.getParsedQuery(), newExecutedQuery.getModel());
			this.jsonDBresult.insert(queryResult);
		} catch (RuntimeException e) {
			System.out.println(e.getMessage());
			throw new ExectutionFailedErrorException(e.getMessage());
		}
		
		String id = newExecutedQuery.getId();
		this.jsonDBquery.insert(newExecutedQuery);
		return this.jsonDBquery.findById(id, ExecutedQuery.class);
	}
}
