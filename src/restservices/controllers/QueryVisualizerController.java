package restservices.controllers;

import java.io.IOException;

import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import io.jsondb.JsonDBTemplate;
import jsondb.JsonDB;
import query.SelectiveQuery;
import queryVisualizer.QueryVisualizer;
import queryVisualizer.D3Classes.GraphWrapper;
import restservices.executeQueryService.ExecutedQuery;
import restservices.executeQueryService.ExecutedQueryNotFoundException;

@RestController
public class QueryVisualizerController {
	private final JsonDBTemplate jsonDBquery;

	public QueryVisualizerController() {
		JsonDB jsonDBQ = new JsonDB("jsondbfiles", "restservices.executeQueryService", "ExecutedQueryInstances");
		this.jsonDBquery = jsonDBQ.getTemplate();
	}

	@CrossOrigin(origins = "http://localhost:3000")
	@PostMapping("/visualizeQuery/{id}")
	public GraphWrapper visualizeQuery(@PathVariable String id) throws IOException {
		this.jsonDBquery.reLoadDB();
		ExecutedQuery result = this.jsonDBquery.findById(id, ExecutedQuery.class);
		if (result == null) {
			throw new ExecutedQueryNotFoundException(id);
		}
		SelectiveQuery selectiveQuery = new SelectiveQuery(result.getOriginalQuery());
		QueryVisualizer visualizer = new QueryVisualizer(selectiveQuery.getFoldBlocks());
		return visualizer.getD3Graph();
	}

}
