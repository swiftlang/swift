// RUN: %target-run-simple-swift(-Xfrontend -sil-verify-all ) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -Xfrontend -sil-verify-all ) | %FileCheck %s

// REQUIRES: executable_test, asserts

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// REQUIRES: rdar125805695

protocol P {
  func speak()
}

extension P {
  func speak() { print("hello") }
}

struct Nonescapable: ~Escapable {}
struct Ordinary {}

struct Dog<T: ~Escapable>: Escapable {}
extension Dog: P where T: Escapable {}

func attemptCall(_ a: Any) {
  if let value = a as? P {
    value.speak()
    return
  }
  print("failed to cast (attemptCall)")
}

defer { main({}) }
func main(_ noEscapeFunc: () -> Void) {
  // CHECK: hello
  attemptCall(Dog<Ordinary>())

  // CHECK: failed to cast (attemptCall)
  attemptCall(Dog<Nonescapable>())

  // CHECK: function types
  print("function types")

  // CHECK: hello
  attemptCall(Dog<() -> Void>())

  // CHECK: failed to cast (attemptCall)
  func doFuncCall<F>(_: F.Type) {
    attemptCall(Dog<F>())
  }
  // This is the mangled name for a non-escaping Swift function type.
  let fnType = _typeByName("yyXE")!
  _openExistential(fnType, do: doFuncCall)
}

