// This class is not in use. This can be used to track changes in certains directories. 
// After all, there is no guarantee that the modification of a file in the directory has finished before this class notifies the change.
package resultparser;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.WatchEvent;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import static java.nio.file.StandardWatchEventKinds.*;

public class QueryExecutionListener {

	private final WatchService watcher;

	public QueryExecutionListener() throws IOException {
		this.watcher = FileSystems.getDefault().newWatchService();
	}
	
	public boolean notifyWhenExecutionFinished() throws InterruptedException, IOException {

		Path path = FileSystems.getDefault().getPath("C:\\Users\\Valter Uotila\\Desktop\\MultiModelQueryCompiler", "output");

		path.register(this.watcher, ENTRY_MODIFY); // ENTRY_CREATE, ENTRY_DELETE options

		WatchKey key;
		while ((key = this.watcher.take()) != null) {
			for (WatchEvent<?> event : key.pollEvents()) {
				System.out.println("Event kind:" + event.kind() + ". File affected: " + event.context());
				final Path changed = (Path) event.context();
				if (changed.endsWith("output")) {
					key.cancel();
					break;
				}
			}
			key.reset();

			break;
		}
		return true;
	}

}
