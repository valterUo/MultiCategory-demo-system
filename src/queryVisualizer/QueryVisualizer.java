package queryVisualizer;

import java.util.ArrayList;

import query.FoldBlock;
import query.LambdaFunction;
import queryVisualizer.D3Classes.D3Edge;
import queryVisualizer.D3Classes.D3Graph;
import queryVisualizer.D3Classes.D3Node;
import queryVisualizer.D3Classes.GraphWrapper;

public class QueryVisualizer {
	private final ArrayList<FoldBlock> query;
	private GraphWrapper finalGraph;

	public QueryVisualizer(ArrayList<FoldBlock> query) {
		this.query = query;
		this.finalGraph = new GraphWrapper();
		this.finalGraph.setFoldDiagram(createFoldBlockGraph());
	}

	public D3Graph createFoldBlockGraph() {
		D3Graph graph = new D3Graph();
		ArrayList<D3Node> previousFoldNodes = new ArrayList<>();

		for (int i = 0; i < this.query.size(); i++) {

			FoldBlock fold = this.query.get(i);
			String associatedVariable = null;
			Boolean resultConnectionCreated = false;

			if (i > 0) {
				associatedVariable = this.query.get(i - 1).getAssociatedVariable();
			}

			D3Node foldNode = new D3Node(fold.getFoldFunction());
			graph.addNode(foldNode);

			int j = 0;
			for (LambdaFunction lambda : fold.getLambdaFunctions()) {
				D3Node lambdaNode = new D3Node("Lambda " + j);
				String id = lambdaNode.getId();
				D3Graph lambdaGraph = createLambdaFunctionGraph(id, lambda);
				this.finalGraph.addLambdaFunction(lambdaGraph);
				if (associatedVariable != null
						&& lambda.flattenLambdaFunction().contains(" " + associatedVariable + " ") && i > 0) {
					D3Node previousFoldNode = previousFoldNodes.get(previousFoldNodes.size() - 1);
					graph.addLink(previousFoldNode, new D3Edge("Result " + i), lambdaNode);
					resultConnectionCreated = true;
				}
				graph.addLink(lambdaNode, new D3Edge("Lambda " + j), foldNode);
				j++;
			}

			graph.addLink(new D3Node(fold.getNilCollection()), new D3Edge("Nil"), foldNode);
			graph.addLink(new D3Node(fold.getSourceCollectionName()), new D3Edge(fold.getSourceModel()), foldNode);

			if (!resultConnectionCreated && i > 0) {
				D3Node previousFoldNode = previousFoldNodes.get(previousFoldNodes.size() - 1);
				graph.addLink(previousFoldNode, new D3Edge("Result " + i), foldNode);
			}

			previousFoldNodes.add(foldNode);
		}
		return graph;
	}

	private D3Graph createLambdaFunctionGraph(String id, LambdaFunction lambda) {
		D3Graph graph = new D3Graph();
		System.out.println(lambda);
		return graph;	
	}
	
	//public D3Graph

	public GraphWrapper getD3Graph() {
		return this.finalGraph;
	}

}
