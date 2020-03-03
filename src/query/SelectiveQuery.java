package query;

import java.util.ArrayList;

import codeGenerator.CodeGenerator;
import codeGenerator.LambdaFunctionModifier;
import scanner.SelectiveQueryScanner;

public class SelectiveQuery implements QueryInterface {
	private final String queryString;
	private final String targetModel;
	private ArrayList<QueryBlock> queryBlocks;
	private ArrayList<QueryBlock> translatedQueryBlocks;
	private ArrayList<FoldBlock> foldBlocks;

	public SelectiveQuery(String queryString) {
		LambdaFunctionModifier lambdaMod = new LambdaFunctionModifier();
		this.queryString = queryString;
		this.queryBlocks = parseLetBeInBlocks(queryString);
		this.targetModel = this.queryBlocks.get(this.queryBlocks.size() - 1).getTargetModel();
		this.translatedQueryBlocks = lambdaMod.queryBlockModifier(this.queryBlocks);
		this.foldBlocks = generateFoldBlocks();
	}

	public ArrayList<QueryBlock> parseLetBeInBlocks(String query) {
		SelectiveQueryScanner scanner = new SelectiveQueryScanner();
		ArrayList<QueryBlock> queryBlocks = new ArrayList<QueryBlock>();
		ArrayList<String> letBeInBlocks = scanner.scanLetBeInBlock(query);
		for (int i = 0; i < letBeInBlocks.size(); i++) {
			String element = letBeInBlocks.get(i).trim();
			if (element.startsWith("QUERY") && i - 2 > 0 && i < letBeInBlocks.size() - 1) {
				String variable = letBeInBlocks.get(i - 2).trim();
				queryBlocks.add(new QueryBlock(element, variable));
			} else if (element.startsWith("QUERY")) {
				queryBlocks.add(new QueryBlock(element, null));
			}
		}
		return queryBlocks;
	}

	public ArrayList<QueryBlock> getQueryBlocks() {
		return this.queryBlocks;
	}
	
	public ArrayList<FoldBlock> generateFoldBlocks() {
		ArrayList<FoldBlock> folds = new ArrayList<FoldBlock>();
		for (QueryBlock query : this.translatedQueryBlocks) {
			CodeGenerator gen = new CodeGenerator(query);
			String variable = query.getAssociatedVariable();
			FoldBlock fold = gen.getFold();
			fold.setAssociatedVariable(variable);
			folds.add(fold);
		}
		return folds;
	}

	public ArrayList<FoldBlock> getFoldBlocks() {
		return this.foldBlocks;
	}

	@Override
	public void printParseTree() {
		System.out.println("Whole query: " + this.queryString + "\n");
		for (QueryBlock block : this.queryBlocks) {
			System.out.println(block.toString() + "\n");
		}
	}

	@Override
	public String getHaskellCode() {
		String result = "";
		for(FoldBlock fold : this.foldBlocks) {
			if (fold.getAssociatedVariable() != null) {
				result += "let " + fold.getAssociatedVariable() + " = "
						+ fold.toString() + " in ";
			} else {
				result += fold.toString();
			}
		}
		return result.replaceAll("\\s{2,}", " ").trim();
	}

	@Override
	public String getQuery() {
		return this.queryString;
	}

	public String getTargetModel() {
		return this.targetModel;
	}

}
