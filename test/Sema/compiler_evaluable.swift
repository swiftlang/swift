// RUN: %target-typecheck-verify-swift

protocol AProtocol {
  @compilerEvaluable
  func protocolFunc() // expected-error{{@compilerEvaluable functions not allowed here}}
}

extension AProtocol {
  @compilerEvaluable
  func extensionFunc() {} // expected-error{{@compilerEvaluable functions not allowed here}}
}

class AClass {
  @compilerEvaluable
  init() {} // expected-error{{@compilerEvaluable functions not allowed here}}

  @compilerEvaluable
  func classFunc() {} // expected-error{{@compilerEvaluable functions not allowed here}}
}

struct AStruct {
  @compilerEvaluable
  init() {}

  @compilerEvaluable
  func structFunc() {}

  var structProperty: Int {
    @compilerEvaluable
    get {
      return 42
    }

    @compilerEvaluable
    set(prop) {}
  }
}

func aFunction() {
  @compilerEvaluable
  func functionFunc() {}
}

@compilerEvaluable
func funcTopLevel() {}

@compilerEvaluable
func funcWithLoop() -> Int {
  var x = 1
  while (x < 10) { // expected-error{{loops not allowed in @compilerEvaluable functions}}
    x += 1
  }
  return x
}
