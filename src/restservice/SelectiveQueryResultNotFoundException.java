package restservice;

@SuppressWarnings("serial")
class SelectiveQueryResultNotFoundException extends RuntimeException {

	SelectiveQueryResultNotFoundException(Long id) {
		super("Could not return the result " + id);
	}
}