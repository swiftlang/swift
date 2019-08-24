// Note that for the test to be effective, each of these enums must only have
// its Equatable or Hashable conformance referenced /once/ in the primary file.
enum FromOtherFile : Error {
  case A
}
enum AlsoFromOtherFile : Error {
  case A
}
enum YetAnotherFromOtherFile : Error {
  case A
}
