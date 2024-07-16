// RUN: %empty-directory(%t)

// RUN: %target-build-swift -D CRASH %s -o %t/crash
// RUN: %target-codesign %t/crash

// RUN: %target-build-swift %s -o %t/exe
// RUN: %target-codesign %t/exe

// RUN: %target-run %t/exe | %FileCheck %s
// RUN: %target-run not --crash %t/crash

// REQUIRES: executable_test

// Simulators and devices don't appear to have the 'not' binary in their PATH
// to handle tests that intentionally crash such as this.
// UNSUPPORTED: DARWIN_SIMULATOR={{.*}}
// UNSUPPORTED: remote_run || device_run

enum Maybe<Wrapped: ~Copyable>: ~Copyable {
  case some(Wrapped)
  case none
}
extension Maybe: Copyable where Wrapped: Copyable {}

struct NC: ~Copyable {
  let data: Int
}

class Protected<T: ~Copyable> {
  var field: Maybe<T>
  init(_ t: consuming T) {
    self.field = .some(t)
  }
}

class Dangerous<T: ~Copyable> {
  var field: Optional<T>
  init(_ t: consuming T) {
    self.field = .some(t)
  }
}

func printFields<T: Copyable>(_ val: T) {
  let mirror = Mirror.init(reflecting: val)
  mirror.children.forEach { print($0.label ?? "", $0.value) }
}

defer { test() }
func test() {
#if CRASH
  printFields(Dangerous(NC(data: 22)))
#else
  printFields(Protected("oreo"))        // CHECK: field ()
  printFields(Protected(NC(data: 11)))  // CHECK: field ()
  printFields(Dangerous("spots"))       // CHECK: field Optional("spots")
#endif
}
