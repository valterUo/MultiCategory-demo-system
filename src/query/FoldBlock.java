package query;

import java.util.ArrayList;

public class FoldBlock {
	private String foldFunction;
	private String nilCollection;
	private ArrayList<LambdaFunction> lambdaFunctions;
	private String associatedVariable;
	private String sourceModel;
	private String targetModel;
	private String sourceCollectionName;
	private String prefixes;
	private String suffixes;

	public FoldBlock() {
		this.foldFunction = null;
		this.nilCollection = null;
		this.lambdaFunctions = null;
		this.associatedVariable = null;
		this.sourceCollectionName = null;
		this.sourceModel = null;
		this.targetModel = null;
		this.prefixes = "";
		this.suffixes = "";
	}

	public FoldBlock(String foldFunction, String nilCollection, ArrayList<LambdaFunction> lambdaFunctions, String associatedVariable) {
		this.foldFunction = foldFunction;
		this.nilCollection = nilCollection;
		this.lambdaFunctions = lambdaFunctions;
		this.associatedVariable = associatedVariable;
	}
	
	public String getFoldFunction() {
		return this.foldFunction;
	}

	public void setFoldFunction(String foldFunction) {
		this.foldFunction = foldFunction;
	}

	public String getNilCollection() {
		return this.nilCollection;
	}

	public void setNilCollection(String nilCollection) {
		this.nilCollection = nilCollection;
	}

	public ArrayList<LambdaFunction> getLambdaFunctions() {
		return this.lambdaFunctions;
	}

	public void setLambdaFunctions(ArrayList<LambdaFunction> lambdaFunctions) {
		this.lambdaFunctions = lambdaFunctions;
	}

	public String getAssociatedVariable() {
		return this.associatedVariable;
	}

	public void setAssociatedVariable(String associatedVariable) {
		this.associatedVariable = associatedVariable;
	}
	
	public String getSourceModel() {
		return this.sourceModel;
	}

	public void setSourceModel(String sourceModel) {
		this.sourceModel = sourceModel;
	}

	public String getTargetModel() {
		return this.targetModel;
	}

	public void setTargetModel(String targetModel) {
		this.targetModel = targetModel;
	}

	public String getSourceCollectionName() {
		return this.sourceCollectionName;
	}

	public void setSourceCollectionName(String sourceCollectionName) {
		this.sourceCollectionName = sourceCollectionName;
	}

	public String getPrefixes() {
		return this.prefixes;
	}

	public void setPrefixes(String prefixes) {
		this.prefixes = prefixes;
	}

	public String getSuffixes() {
		return this.suffixes;
	}

	public void setSuffixes(String suffixes) {
		this.suffixes = suffixes;
	}
	
	public String toString() {
		String fold = "";
		String lambdaf = "";
		for(LambdaFunction lambda : this.lambdaFunctions) {
			lambdaf += "(" + lambda.flattenLambdaFunction() + ") ";
		}
		if(this.foldFunction.equals("foldg")) {
			fold += this.foldFunction + " " + this.nilCollection + " " + lambdaf + " " + this.sourceCollectionName;
		} else {
			fold += this.foldFunction + " " + lambdaf + " " + this.nilCollection + " " + this.sourceCollectionName;
		}
		return this.prefixes + fold + this.suffixes;
	}

}
