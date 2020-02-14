package resultparser;

@SuppressWarnings("serial")
public class HaskellExecutionFailureExeception extends RuntimeException {
	HaskellExecutionFailureExeception(String message) {
		super("Haskell execution failed with error message: " + message);
	}
}

