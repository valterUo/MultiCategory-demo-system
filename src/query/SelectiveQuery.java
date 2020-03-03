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
		//this.foldBlocks = generateFoldBlocks();
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
	
//	public ArrayList<FoldBlock> generateFoldBlocks() {
//		
//	}

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
		for (QueryBlock query : this.translatedQueryBlocks) {
			CodeGenerator gen = new CodeGenerator(query);
			if (query.getAssociatedVariable() != null) {
				result += "let " + query.getAssociatedVariable() + " = "
						+ gen.getFoldFunction() + " in ";
			} else {
				result += gen.getFoldFunction();
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
