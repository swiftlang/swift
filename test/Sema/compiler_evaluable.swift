// RUN: %target-typecheck-verify-swift

protocol AProtocol {
  @compilerEvaluable
  func protocolFunc() // expected-error{{@compilerEvaluable functions not allowed here}}
}

extension AProtocol {
  @compilerEvaluable
  func extensionFunc() {}
}

class AClass {
  @compilerEvaluable
  init() {} // expected-error{{@compilerEvaluable functions not allowed here}}

  @compilerEvaluable
  func classFunc() {} // expected-error{{@compilerEvaluable functions not allowed here}}
}

extension AClass {
  @compilerEvaluable
  func extensionFunc() {} // expected-error{{@compilerEvaluable functions not allowed here}}
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

  subscript(i: Int) -> Int {
    @compilerEvaluable
    get {}

    @compilerEvaluable
    set(v) {}
  }
}

extension AStruct {
  @compilerEvaluable
  func extensionFunc() {}
}

struct AGenericStruct<T> {
  @compilerEvaluable
  init() {}

  @compilerEvaluable
  func structFunc() {}
}

extension AGenericStruct {
  @compilerEvaluable
  func extensionFunc() {}
}

enum AEnum {
  case thing1
  case thing2

  @compilerEvaluable
  func enumFunc() {}
}

extension AEnum {
  @compilerEvaluable
  func extensionFunc() {}
}

enum AGenericEnum<T> {
  case thing1
  case thing2

  @compilerEvaluable
  func enumFunc() {}
}

extension AGenericEnum {
  @compilerEvaluable
  func extensionFunc() {}
}

func aFunction() {
  @compilerEvaluable
  func functionFunc() {}
}

func aGenericFunction<T>(t: T) {
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
