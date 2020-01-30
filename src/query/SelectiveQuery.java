package query;

import java.util.ArrayList;

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
		for (String element : letBeInBlocks) {
			// System.out.println("LET BE IN element: " + element);
			if(element.trim().startsWith("QUERY")) {
				queryBlocks.add(new QueryBlock(element));
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
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getQuery() {
		return this.queryString;
	}

}
