package restservices.selectiveQueryService;

@SuppressWarnings("serial")
public class SelectiveQueryResultNotFoundException extends RuntimeException {
	public SelectiveQueryResultNotFoundException(String id) {
		super("Could not return the result " + id);
	}
}