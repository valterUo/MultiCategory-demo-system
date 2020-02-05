package process;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.Scanner;
import java.util.function.Consumer;

public class StreamGobbler extends Thread {
	OutputStream is;
	String query;
	File inputFile;
	
	public StreamGobbler(OutputStream is) {
		this.is = is;
		this.query = null;
		this.inputFile = null;
	}

	public StreamGobbler(OutputStream is, String query) {
		this.is = is;
		this.query = query;
		this.inputFile = null;
	}

	public StreamGobbler(OutputStream is, File inputFile) {
		this.is = is;
		this.query = null;
		this.inputFile = inputFile;
	}

	public StreamGobbler(OutputStream is, String query, File inputFile) {
		this.is = is;
		this.query = query;
		this.inputFile = inputFile;
	}

	@Override
	public void run() {
		try {
			OutputStreamWriter isr = new OutputStreamWriter(this.is);
			BufferedWriter br = new BufferedWriter(isr);
			if (this.query != null) {
				br.write(this.query + "\n");
				br.flush();
			}
			if (this.inputFile != null) {
				@SuppressWarnings("resource")
				Scanner scanner = new Scanner(this.inputFile);
				while (scanner.hasNextLine()) {
					String line = scanner.nextLine();
					System.out.println(line);
					br.write(line + "\n");
					br.flush();
				}
			}
		} catch (IOException ioe) {
			ioe.printStackTrace();
		}
	}

	public void executeQuery(String query) {
		try {
			OutputStreamWriter isr = new OutputStreamWriter(this.is);
			BufferedWriter br = new BufferedWriter(isr);
			br.write(query + "\n");
			br.flush();
		} catch (IOException ioe) {
			ioe.printStackTrace();
		}
	}

	public File getInputFile() {
		return this.inputFile;
	}

	public void setInputFile(File inputFile) {
		this.inputFile = inputFile;
	}

	public String getQuery() {
		return this.query;
	}

	public void setQuery(String query) {
		this.query = query;
	}
}