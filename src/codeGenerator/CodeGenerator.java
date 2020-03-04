package codeGenerator;

import query.FoldBlock;
import query.QueryBlock;

public class CodeGenerator {
	private FoldBlock fold;

	public CodeGenerator(QueryBlock query) {
		this.fold = generateFoldFunctionFromQueryBlock(query);
	}

	public FoldBlock getFold() {
		return fold;
	}

	public FoldBlock generateFoldFunctionFromQueryBlock(QueryBlock query) {
		FoldBlock fold = new FoldBlock();
		try {
			String sourceCollectionName = query.getSourceCollectionName();
			String sourceCollectionModel = query.getSourceCollectionModel();
			String targetCollectionModel = query.getTargetModel();
			fold.setSourceCollectionName(sourceCollectionName);
			fold.setSourceModel(sourceCollectionModel);
			fold.setTargetModel(targetCollectionModel);
			fold.setLambdaFunctions(query.getLambdaFunctions());

			switch (sourceCollectionModel) {
			case "relational":
				fold.setFoldFunction("foldr");
				break;
			case "algebraic graph":
				fold.setFoldFunction("foldg");
				break;
			case "xml":
				fold.setFoldFunction("foldr");
				break;
			case "json":
				fold.setFoldFunction("foldr");
				break;
			case "rdf":
				fold.setFoldFunction("foldrdf");
				break;
			case "nimblegraph":
				fold.setFoldFunction("foldNimble");
				break;
			default:
				System.out.println("No source collection model match!");
			}

			switch (targetCollectionModel) {
			case "relational":
				fold.setNilCollection("[]");
				break;
			case "algebraic graph":
				fold.setNilCollection("Algebra.Graph.empty");
				break;
			case "xml":
				fold.setNilCollection("[]");
				break;
			case "json":
				fold.setNilCollection("[]");
				break;
			case "rdf":
				fold.setNilCollection("RDF.empty");
				break;
			case "nimblegraph":
				fold.setNilCollection("emptyNimbleGraph");
				break;
			default:
				System.out.println("no match");
			}

			if (targetCollectionModel.equals("rdf")) {
				fold.setPrefixes("(");
				fold.setSuffixes(" :: RDF TList)");
			}

		} catch (Exception e) {
			System.out.println("Error! Source collection model was not found! Error: " + e);
		}

		return fold;
	}

	public String getFoldFunction() {
		return this.fold.toString();
	}
}
