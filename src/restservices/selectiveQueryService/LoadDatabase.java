//package restservices.selectiveQueryService;
//
//import org.springframework.boot.CommandLineRunner;
//import org.springframework.context.annotation.Bean;
//import org.springframework.context.annotation.Configuration;
//
//import lombok.extern.slf4j.Slf4j;
//
//@Configuration
//@Slf4j
//class LoadDatabase {
//
//	@Bean
//	CommandLineRunner initDatabase(SelectiveQueryResultRepository repository) {
//		return args -> {
//			// Create some examples for testing
//			repository.save(new SelectiveQueryResult("query","query", "query"));
//			repository.save(new SelectiveQueryResult("query2","query2", "query2"));
//		};
//	}
//}