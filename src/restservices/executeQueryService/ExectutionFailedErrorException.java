package restservices.executeQueryService;

@SuppressWarnings("serial")
public class ExectutionFailedErrorException extends RuntimeException {
	public ExectutionFailedErrorException(String message) {
		super("The exectution failed with error message: " + message);
	}
}