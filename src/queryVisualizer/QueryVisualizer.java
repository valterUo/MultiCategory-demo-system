package queryVisualizer;

import java.util.ArrayList;
import java.util.List;

import collectionMetaData.DecodeMetaData;
import query.FoldBlock;
import query.LambdaFunction;
import query.Pair;
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
		ArrayList<D3Node> inputVariables = new ArrayList<D3Node>();

		int i = 0;
		for (i = 0; i < lambda.getTokens().size(); i++) {
			String token = lambda.getTokens().get(i);
			if (token.contains("->")) {
				break;
			}
			inputVariables.add(new D3Node(token.replaceAll("\\\\", "")));
		}
		// System.out.println(lambda.getTokens().subList(i + 1,
		// lambda.getTokens().size()));
		Pair<D3Node, D3Graph> result = parseLambdaFunction(lambda.getTokens().subList(i + 1, lambda.getTokens().size()),
				lambda.getVariables());
		D3Graph finalGraph = result.snd;
		// System.out.println(finalGraph);
		finalGraph.addLink(result.fst, new D3Edge("output"), new D3Node("Result"));
		// System.out.println(finalGraph);
		return finalGraph;
	}

	public Pair<D3Node, D3Graph> parseLambdaFunction(List<String> tokens, ArrayList<String> variables) {
		if (tokensContainVariable(tokens, variables)) {

			if (tokens.get(0).contains("if")) {
				int thenIndex = tokens.indexOf("then");
				int elseIndex = tokens.indexOf("else");
				List<String> ifThen = tokens.subList(1, thenIndex);
				List<String> thenElse = tokens.subList(thenIndex + 1, elseIndex);
				List<String> elseEnd = tokens.subList(elseIndex + 1, tokens.size());
				Pair<D3Node, D3Graph> subGraph1 = parseLambdaFunction(ifThen, variables);
				Pair<D3Node, D3Graph> subGraph2 = parseLambdaFunction(thenElse, variables);
				Pair<D3Node, D3Graph> subGraph3 = parseLambdaFunction(elseEnd, variables);
				// System.out.println(subGraph1.snd);
				subGraph1.snd.union(subGraph2.snd);
				subGraph1.snd.union(subGraph3.snd);
				D3Node ifMainNode = new D3Node("if a then b else c");
				subGraph1.snd.addLink(subGraph1.fst, new D3Edge("a"), ifMainNode);
				subGraph1.snd.addLink(subGraph2.fst, new D3Edge("b"), ifMainNode);
				subGraph1.snd.addLink(subGraph3.fst, new D3Edge("c"), ifMainNode);
				return new Pair<D3Node, D3Graph>(ifMainNode, subGraph1.snd);
			}
			if (!tokens.contains("if")) {
				String operator = null;
				if (tokens.contains("==")) {
					operator = "==";
				} else if (tokens.contains(">")) {
					operator = ">";
				} else if (tokens.contains("<")) {
					operator = "<";
				}
				if (operator != null) {
					int operatorIndex = tokens.indexOf(operator);
					List<String> RHS = tokens.subList(0, operatorIndex);
					List<String> LHS = tokens.subList(operatorIndex + 1, tokens.size());
					D3Node operatorNode = new D3Node("a" + operator + "b");
					Pair<D3Node, D3Graph> RHSGraph = parseLambdaFunction(RHS, variables);
					Pair<D3Node, D3Graph> LHSGraph = parseLambdaFunction(LHS, variables);
					RHSGraph.snd.union(LHSGraph.snd);
					RHSGraph.snd.addLink(RHSGraph.fst, new D3Edge("b"), operatorNode);
					RHSGraph.snd.addLink(LHSGraph.fst, new D3Edge("a"), operatorNode);
					return new Pair<D3Node, D3Graph>(operatorNode, RHSGraph.snd);
				}
			}
			if (!tokens.contains("if") && !tokens.contains("==") && !tokens.contains(">") && !tokens.contains("<")) {
				DecodeMetaData metaData = new DecodeMetaData();
				ArrayList<String> morphisms = metaData.getMorphismsForCategoricalView();
				for (int i = 0; i < tokens.size(); i++) {
					String token = tokens.get(i);
					if (morphisms.contains(token)) {
						String nodeName = token;
						// System.out.println(token);
						int j = i + 1;
						while (i + 1 < tokens.size() && variables.contains(tokens.get(i + 1))) {
							nodeName += " " + tokens.get(i + 1);
							i++;
						}
						List<String> leftAfterMorphism = tokens.subList(j, tokens.size());
						Pair<D3Node, D3Graph> morphismGraph = parseLambdaFunction(leftAfterMorphism, variables);
						D3Node morphismNode = new D3Node(nodeName);
						morphismGraph.snd.addLink(morphismGraph.fst, new D3Edge("morphism link"), morphismNode);
						return new Pair<D3Node, D3Graph>(morphismNode, morphismGraph.snd);
					}
				}
			}
			if (tokens.contains("union") || tokens.contains("connect") || tokens.contains("overlay")
					|| tokens.contains("rdfUnion") || tokens.contains("nimbleGraphUnion")) {
				String function = "";
				int functionIndex = 0;
				for (String token : tokens) {
					if (token.contains("union")) {
						function = "union";
						break;
					} else if (token.contains("connect")) {
						function = "connect";
						break;
					} else if (token.contains("overlay")) {
						function = "overlay";
						break;
					} else if (token.contains("rdfUnion")) {
						function = "rdfUnion";
						break;
					} else if (token.contains("nimbleGraphUnion")) {
						function = "nimbleGraphUnion";
						break;
					}
					functionIndex++;
				}
				
				if (function != "") {
					D3Node functionNode = new D3Node(function + " a b");
					List<String> firstArgument = tokens.subList(functionIndex + 1, functionIndex + 2);
					List<String> secondArgument = tokens.subList(functionIndex + 2, tokens.size());
					Pair<D3Node, D3Graph> functionGraphFirstArgument = parseLambdaFunction(firstArgument,
							variables);
					Pair<D3Node, D3Graph> functionGraphSecondArgument = parseLambdaFunction(secondArgument,
							variables);
					functionGraphFirstArgument.snd.addLink(functionGraphFirstArgument.fst, new D3Edge("argument"),
							functionNode);
					functionGraphFirstArgument.snd.addLink(functionGraphSecondArgument.fst, new D3Edge("argument"),
							functionNode);
					return new Pair<D3Node, D3Graph>(functionNode, functionGraphFirstArgument.snd);
				}

			}
			if (tokensContainOnlyVariables(tokens, variables)) {
				D3Graph initialGraph = new D3Graph();
				if (tokens.size() == 1) {
					D3Node constantNode = new D3Node(tokens.get(0));
					initialGraph.addNode(constantNode);
					return new Pair<D3Node, D3Graph>(constantNode, initialGraph);
				} else {
					for (String variable : tokens) {
						// System.out.println("Variable: ");
						// System.out.println(variable);
						ArrayList<String> variableList = new ArrayList<>();
						variableList.add(variable);
						Pair<D3Node, D3Graph> variableGraph = parseLambdaFunction(variableList, variables);
						initialGraph.union(variableGraph.snd);
					}
				}
			}

			System.out.println("No match for the tokens: ");
			System.out.println(tokens);
		}

		String constant = "";
		for (String token : tokens) {
			constant += " " + token;
		}
		D3Node constantNode = new D3Node(constant);
		D3Graph initialGraph = new D3Graph();
		initialGraph.addNode(constantNode);
		return new Pair<D3Node, D3Graph>(constantNode, initialGraph);
	}

	public boolean tokensContainVariable(List<String> tokens, ArrayList<String> variables) {
		for (String token : tokens) {
			for (String variable : variables) {
				if (token.contains(variable)) {
					return true;
				}
			}
		}
		return false;
	}

	public boolean tokensContainOnlyVariables(List<String> tokens, ArrayList<String> variables) {
		for (String token : tokens) {
			if (!variables.contains(token)) {
				return false;
			}
		}
		return true;
	}

	public GraphWrapper getD3Graph() {
		return this.finalGraph;
	}

}
