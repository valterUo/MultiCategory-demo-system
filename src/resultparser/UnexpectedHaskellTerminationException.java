package resultparser;

@SuppressWarnings("serial")
public class UnexpectedHaskellTerminationException extends RuntimeException {
	UnexpectedHaskellTerminationException(String message) {
		super("Haskell execution terminated unexpectedly with error message: " + message);
	}
}

