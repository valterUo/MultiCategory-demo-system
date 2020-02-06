package restservice;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;

@ControllerAdvice
class SelectiveQueryResultControllerNotFoundAdvice {

  @ResponseBody
  @ExceptionHandler(SelectiveQueryResultNotFoundException.class)
  @ResponseStatus(HttpStatus.NOT_FOUND)
  String employeeNotFoundHandler(SelectiveQueryResultNotFoundException ex) {
    return ex.getMessage();
  }
}