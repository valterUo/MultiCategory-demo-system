package restservices.executeQueryService;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;

@ControllerAdvice
class ExecutedQueryResultNotFoundExceptionAdvice {

  @ResponseBody
  @ExceptionHandler(ExecutedQueryNotFoundException.class)
  @ResponseStatus(HttpStatus.NOT_FOUND)
  String employeeNotFoundHandler(ExecutedQueryNotFoundException ex) {
    return ex.getMessage();
  }
}
