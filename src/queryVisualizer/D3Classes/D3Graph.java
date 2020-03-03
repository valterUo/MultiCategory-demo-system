package queryVisualizer.D3Classes;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class D3Graph {
	private String id;
	private List<D3Node> nodes;
	private List<D3Edge> links;
	
	public D3Graph() {
		this.id = UUID.randomUUID().toString();
		this.nodes = new ArrayList<>();
		this.links = new ArrayList<>();
	}

	public D3Graph(List<D3Node> nodes, List<D3Edge> links) {
		this.id = UUID.randomUUID().toString();
		this.nodes = nodes;
		this.links = links;
	}
	
	public D3Graph(List<D3Node> nodes, List<D3Edge> links, String id) {
		this.id = id;
		this.nodes = nodes;
		this.links = links;
	}
	
	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}


	public List<D3Node> getNodes() {
		return this.nodes;
	}

	public void setNodes(List<D3Node> nodes) {
		this.nodes = nodes;
	}

	public List<D3Edge> getLinks() {
		return this.links;
	}

	public void setLinks(List<D3Edge> links) {
		this.links = links;
	}
	
	public void addNode(D3Node node) {
		this.nodes.add(node);
	}
	
	public void addLink(D3Node startNode, D3Edge link, D3Node endNode) {
		Integer startIndex = this.nodes.indexOf(startNode);
		Integer endIndex = this.nodes.indexOf(endNode);
		if(startIndex == -1 && endIndex == -1) {
			this.nodes.add(startNode);
			this.nodes.add(endNode);
			startIndex = this.nodes.indexOf(startNode);
			endIndex = this.nodes.indexOf(endNode);
			link.setSource(startIndex);
			link.setTarget(endIndex);
			this.links.add(link);
		} else if(startIndex == -1) {
			this.nodes.add(startNode);
			startIndex = this.nodes.indexOf(startNode);
			link.setSource(startIndex);
			link.setTarget(endIndex);
			this.links.add(link);
		} else if(endIndex == -1) {
			this.nodes.add(endNode);
			endIndex = this.nodes.indexOf(endNode);
			link.setSource(startIndex);
			link.setTarget(endIndex);
			this.links.add(link);
		} else {
			link.setSource(startIndex);
			link.setTarget(endIndex);
			this.links.add(link);
		}
	}
	
	public void addLink(D3Edge link) {
		this.links.add(link);
	}
	
	public String toString() {
		String result = "nodes: ";
		for(D3Node node : this.nodes) {
			result += node.getName() + ", ";
		}
		result += "\n links: ";
		for(D3Edge link : this.links) {
			result += "{ source: " + link.getSource() + ", target: " + link.getTarget() + ", name: " + link.getName() + " }";
		}
		return result;
	}
}
