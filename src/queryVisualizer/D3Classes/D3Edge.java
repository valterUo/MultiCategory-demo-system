package queryVisualizer.D3Classes;

public class D3Edge {
	private int source;
	private int target;
	private String name;
	
	public D3Edge(int source, int target, String name) {
		this.source = source;
		this.target = target;
		this.name = name;
	}
	
	public D3Edge(String name) {
		this.source = -1;
		this.target = -1;
		this.name = name;
	}

	public int getSource() {
		return source;
	}

	public void setSource(int source) {
		this.source = source;
	}

	public int getTarget() {
		return target;
	}

	public void setTarget(int target) {
		this.target = target;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
}
