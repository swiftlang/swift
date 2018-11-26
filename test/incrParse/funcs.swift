// RUN: %empty-directory(%t)
// RUN: %validate-incrparse %s --test-case NO_CHANGES
// RUN: %validate-incrparse %s --test-case ADD_FUNC_PARENS
// RUN: %validate-incrparse %s --test-case ADD_OPENING_BRACE
// RUN: %validate-incrparse %s --test-case REMOVE_FUNC_KEYWORD
// RUN: %validate-incrparse %s --test-case ADD_PARAM_NAME
// RUN: %validate-incrparse %s --test-case ADD_PARAM_TYPE

func start() {}

class Bar

let y = 1

class InvalidFuncDecls {
  func parensAdded<<ADD_FUNC_PARENS<|||()>>> {
  }

  func openingBraceAdded() <<ADD_OPENING_BRACE|||{>>>

  func closingBraceAdded() {

  <<ADD_ClOSING_BRACE|||}>>>

  <<REMOVE_FUNC_KEYWORD<func|||>>> funcKeywordRemoved() {

  }

  func addingParamName(<<ADD_PARAM_NAME<|||arg>>>) {

  }

  func addingParamColon(arg<<ADD_PARAM_COLON<|||:>>>) {
  	
  }

  func addingParamType(arg:<<ADD_PARAM_TYPE<||| String>>>) {
  	
  }
}
