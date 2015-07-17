// Note that for the test to be effective, each of these enums must only have
// its Equatable or Hashable conformance referenced /once/ in the primary file.
enum FromOtherFile: ErrorType {
  case A
}
enum AlsoFromOtherFile: ErrorType {
  case A
}
enum YetAnotherFromOtherFile: ErrorType {
  case A
}
