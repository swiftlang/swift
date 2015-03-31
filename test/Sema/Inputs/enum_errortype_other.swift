// Note that for the test to be effective, each of these enums must only have
// its Equatable or Hashable conformance referenced /once/ in the primary file.
enum FromOtherFile: _ErrorType {
  case A
}
enum AlsoFromOtherFile: _ErrorType {
  case A
}
enum YetAnotherFromOtherFile: _ErrorType {
  case A
}
