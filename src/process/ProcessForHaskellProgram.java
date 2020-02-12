package process;

import java.io.File;
import java.io.IOException;
import java.lang.ProcessBuilder.Redirect;

public class ProcessForHaskellProgram {
	private StreamGobbler gobbler;
	private String haskellProgramFilePath = "C:\\Users\\Valter Uotila\\Desktop\\demo-system-backend-Haskell\\MultiCategory";
	private File output;

	public ProcessForHaskellProgram() throws IOException, InterruptedException {
		this.output = new File("output\\output");
		ProcessBuilder builder = new ProcessBuilder(); //.inheritIO();
		builder.redirectErrorStream(true);
		// builder.redirectInput(Redirect.INHERIT);
		builder.redirectOutput(Redirect.appendTo(this.output));

		builder.command("cmd.exe", "/c", "stack ghci");
		builder.directory(new File(this.haskellProgramFilePath));
		Process process = builder.start();

		this.gobbler = new StreamGobbler(process.getOutputStream());
		this.gobbler.start();

	}

	public ProcessForHaskellProgram(File inputQueries) throws IOException, InterruptedException {

		ProcessBuilder builder = new ProcessBuilder();
		builder.redirectErrorStream(true);
		builder.redirectOutput(Redirect.appendTo(this.output));

		builder.command("cmd.exe", "/c", "stack ghci");
		builder.directory(new File(this.haskellProgramFilePath));
		Process process = builder.start();

		this.gobbler = new StreamGobbler(process.getOutputStream(), inputQueries);
		this.gobbler.start();

	}

	public ProcessForHaskellProgram(String initialQuery) throws IOException, InterruptedException {

		ProcessBuilder builder = new ProcessBuilder();
		builder.redirectErrorStream(true);
		builder.redirectOutput(Redirect.appendTo(this.output));

		builder.command("cmd.exe", "/c", "stack ghci");
		builder.directory(new File(this.haskellProgramFilePath));
		Process process = builder.start();

		this.gobbler = new StreamGobbler(process.getOutputStream(), initialQuery);
		this.gobbler.start();

	}

	public ProcessForHaskellProgram(String initialQuery, File inputQueries) throws IOException, InterruptedException {

		ProcessBuilder builder = new ProcessBuilder();
		builder.redirectErrorStream(true);
		builder.redirectOutput(Redirect.appendTo(this.output));

		builder.command("cmd.exe", "/c", "stack ghci");
		builder.directory(new File(this.haskellProgramFilePath));
		Process process = builder.start();

		this.gobbler = new StreamGobbler(process.getOutputStream(), initialQuery, inputQueries);
		this.gobbler.start();

	}

	public StreamGobbler getStreamGobbler() {
		return this.gobbler;
	}
}
