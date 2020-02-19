package restservices.executeQueryService;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;

@ControllerAdvice
class ExectutionFailedErrorExceptionAdvice {

  @ResponseBody
  @ExceptionHandler(ExectutionFailedErrorException.class)
  @ResponseStatus(HttpStatus.BAD_REQUEST)
  String exectutionFailedErroHandler(ExectutionFailedErrorException ex) {
    return ex.getMessage();
  }
}
