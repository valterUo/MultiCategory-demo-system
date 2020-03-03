package queryVisualizer.D3Classes;

import java.util.UUID;

public class D3Node {
	private String name;
	private final String id;
	
	public D3Node(String name) {
		this.name = name;
		this.id = UUID.randomUUID().toString();
	}
	
	public String getId() {
		return this.id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

}
