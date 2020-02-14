// This class is not in use. It can be used to create a reader that reads a result from a process continuously.
package process;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.stream.Stream;

public class OutputStreamGobbler extends Thread {
	private final InputStream is;
	private BufferedReader br;

	public OutputStreamGobbler(InputStream is) {
		this.is = is;
		this.br = null;
	}

	@Override
	public void run() {
		InputStreamReader isr = new InputStreamReader(is);
		this.br = new BufferedReader(isr);
	}

	public Stream<String> getLines() {
		return this.br.lines();
	}

	public void closeLineStream() {
		this.br.lines().close();
	}
}
