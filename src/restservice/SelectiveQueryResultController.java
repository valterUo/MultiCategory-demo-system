package restservice;

import java.util.List;

import javax.persistence.EntityNotFoundException;

import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.hateoas.*;
import org.springframework.hateoas.CollectionModel;
import org.springframework.hateoas.EntityModel;
import org.springframework.http.ResponseEntity;

@RestController
class SelectiveQueryResultController {

  private final SelectiveQueryResultRepository repository;

  SelectiveQueryResultController(SelectiveQueryResultRepository repository) {
    this.repository = repository;
  }

  // Aggregate root

  @GetMapping("/SelectiveQueryResults")
  List<SelectiveQueryResult> all() {
    return repository.findAll();
  }

  @PostMapping("/SelectiveQueryResults")
  SelectiveQueryResult newSelectiveQueryResult(@RequestBody SelectiveQueryResult newSelectiveQueryResult) {
    return repository.save(newSelectiveQueryResult);
  }

  // Single item
  
  @GetMapping("/SelectiveQueryResults/{id}")
  SelectiveQueryResult one(@PathVariable Long id) {

    return repository.findById(id)
      .orElseThrow(() -> new SelectiveQueryResultNotFoundException(id));
  }

//  @GetMapping("/SelectiveQueryResults/{id}")
//  Resource<SelectiveQueryResult> one(@PathVariable Long id) {
//
//    SelectiveQueryResult SelectiveQueryResult = repository.findById(id)
//      .orElseThrow(() -> new SelectiveQueryResultNotFoundException(id));
//
//    return new Resource<>(SelectiveQueryResult,
//      linkTo(methodOn(SelectiveQueryResultController.class).one(id)).withSelfRel(),
//      linkTo(methodOn(SelectiveQueryResultController.class).all()).withRel("SelectiveQueryResults"));
//  }

  @PutMapping("/SelectiveQueryResults/{id}")
  SelectiveQueryResult replaceSelectiveQueryResult(@RequestBody SelectiveQueryResult newSelectiveQueryResult, @PathVariable Long id) {

    return repository.findById(id)
      .map(SelectiveQueryResult -> {
        SelectiveQueryResult.setResult(newSelectiveQueryResult.getResult());
        SelectiveQueryResult.setModel(newSelectiveQueryResult.getModel());
        SelectiveQueryResult.setParsedQuery(newSelectiveQueryResult.getParsedQuery());
        return repository.save(SelectiveQueryResult);
      })
      .orElseGet(() -> {
        newSelectiveQueryResult.setId(id);
        return repository.save(newSelectiveQueryResult);
      });
  }

  @DeleteMapping("/SelectiveQueryResults/{id}")
  void deleteSelectiveQueryResult(@PathVariable Long id) {
    repository.deleteById(id);
  }
}