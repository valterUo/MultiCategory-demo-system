package process;

import java.util.function.Consumer;

public class ResultConsumer implements Consumer<String> {

	@Override
	public void accept(String t) {
		System.out.println(t);
		
	}

}
