// Note that for the test to be effective, each of these enums must only have
// its Equatable or Hashable conformance referenced /once/ in the primary file.
enum FromOtherFile : ErrorProtocol {
  case A
}
enum AlsoFromOtherFile : ErrorProtocol {
  case A
}
enum YetAnotherFromOtherFile : ErrorProtocol {
  case A
}
