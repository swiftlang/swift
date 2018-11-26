// Note that for the test to be effective, each of these enums must only have
// its Equatable or Hashable conformance referenced /once/ in the primary file.
enum FromOtherFile : String {
// expected-note@-1 {{type declared here}}
  case A = "a"
}
enum AlsoFromOtherFile : Int {
  case A = 0
}
enum YetAnotherFromOtherFile: Float {
  case A = 0.0
}

enum OtherFileNonconforming {
  case A(Int)
}
enum YetOtherFileNonconforming {
// expected-note@-1 {{type declared here}}
  case A(Int)
}

enum GenericOtherFileNonconforming<T> {
// expected-note@-1 {{type declared here}}
    case A(T)
}

protocol ImplierOther: Equatable {}
extension ImpliedMain: ImplierMain {}
enum ImpliedOther: ImplierOther {
    case a(Int)
}
