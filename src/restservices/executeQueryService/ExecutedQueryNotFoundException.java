package restservices.executeQueryService;

@SuppressWarnings("serial")
public class ExecutedQueryNotFoundException extends RuntimeException {
	public ExecutedQueryNotFoundException(String id) {
		super("Could not return the query " + id);
	}
}
