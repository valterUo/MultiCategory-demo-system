package query;

import java.util.ArrayList;

import codeGenerator.CodeGenerator;
import scanner.SelectiveQueryScanner;

public class SelectiveQuery implements QueryInterface {
	private String queryString;
	private ArrayList<QueryBlock> queryBlocks;

	public SelectiveQuery(String queryString) {
		this.queryString = queryString;
		this.queryBlocks = parseLetBeInBlocks(queryString);
	}

	public ArrayList<QueryBlock> parseLetBeInBlocks(String query) {
		SelectiveQueryScanner scanner = new SelectiveQueryScanner();
		ArrayList<QueryBlock> queryBlocks = new ArrayList<QueryBlock>();
		ArrayList<String> letBeInBlocks = scanner.scanLetBeInBlock(query);
		for (int i = 0; i < letBeInBlocks.size(); i++) {
			String element = letBeInBlocks.get(i).trim();
			//System.out.println(element);
			if (element.startsWith("QUERY") && i - 2 > 0 && i < letBeInBlocks.size() - 1) {
				String variable = letBeInBlocks.get(i - 2).trim();
				queryBlocks.add(new QueryBlock(element, variable));
			} else if(element.startsWith("QUERY")) {
				queryBlocks.add(new QueryBlock(element, null));
			}
		}
		return queryBlocks;
	}

	public ArrayList<QueryBlock> getQueryBlocks() {
		return this.queryBlocks;
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
		CodeGenerator gen = new CodeGenerator();
		gen.selectiveQueryModifier(this);
		String result = "";
		for(QueryBlock query : this.queryBlocks) {
			if(query.getAssociatedVariable() != null) {
				result += "let " + query.getAssociatedVariable() + " = " + gen.generateFoldFunctionFromQueryBlock(query) + " in ";
			} else {
				result += gen.generateFoldFunctionFromQueryBlock(query);
			}
			
		}
		return result.trim();
	}

	@Override
	public String getQuery() {
		return this.queryString;
	}

}
