package queryVisualizer.D3Classes;

import java.util.ArrayList;
import java.util.List;

public class GraphWrapper {
	private D3Graph foldDiagram;
	private List<D3Graph> lambdaFunctions;
	
	public GraphWrapper() {
		this.foldDiagram = null;
		this.lambdaFunctions = new ArrayList<>();
	}
	
	public D3Graph getFoldDiagram() {
		return foldDiagram;
	}
	public void setFoldDiagram(D3Graph foldDiagram1) {
		foldDiagram = foldDiagram1;
	}
	public List<D3Graph> getLambdaFunctions() {
		return lambdaFunctions;
	}
	public void setLambdaFunctions(List<D3Graph> lambdaFunctions) {
		this.lambdaFunctions = lambdaFunctions;
	}
	
	public void addLambdaFunction(D3Graph f) {
		this.lambdaFunctions.add(f);
	}
	
	public void prettyPrintGraphWrapper() {
		System.out.println("Main fold diagram:");
		System.out.println();
		System.out.println(this.foldDiagram);
		System.out.println();
		System.out.println("Lambda functions:");
		System.out.println();
		for(D3Graph function : this.lambdaFunctions) {
			System.out.println(function);
		}
	}

}
