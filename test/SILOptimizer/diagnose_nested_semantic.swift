// RUN: %target-swift-frontend -emit-sil -primary-file %s -o /dev/null -verify

@_semantics("array.append_contentsOf")
public func funcA() {
  funcB() // expected-warning {{'@_semantics' function calls non-'@_semantics' function with nested '@_semantics' calls}}
}

func getInt() -> Int {
  return 3
}

// Make sure funcB is not itself a trivial wrapper by calling getInt
// for funcC's argument.
func funcB() {
  funcC(i: getInt())
}

@_semantics("array.mutate_unknown")
func funcC(i: Int) {}
