package resultparser;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import static java.nio.file.StandardWatchEventKinds.*;

public class QueryExecutionListener {
	
	private final WatchService watcher;
	
	public QueryExecutionListener() throws IOException {
		this.watcher = FileSystems.getDefault().newWatchService();
		Path dir = FileSystems.getDefault().getPath("outputLog");
		try {
		    WatchKey key = dir.register(watcher, ENTRY_MODIFY);
		} catch (IOException x) {
		    System.err.println(x);
		}
	}

}
