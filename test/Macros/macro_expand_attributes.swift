// RUN: %empty-directory(%t)
// RUN: %target-build-swift -I %swift-host-lib-dir -L %swift-host-lib-dir -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUNx: %target-swift-frontend -dump-ast -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir %s -module-name MacroUser 2>&1 | %FileCheck --check-prefix CHECK-AST %s
// RUN: %target-typecheck-verify-swift -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir -module-name MacroUser -DTEST_DIAGNOSTICS -swift-version 5
// RUN: %target-build-swift -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir -L %swift-host-lib-dir %s -o %t/main -module-name MacroUser -swift-version 5
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test

// FIXME: Swift parser is not enabled on Linux CI yet.
// REQUIRES: OS=macosx

@attached(memberAttributes) macro wrapAllProperties() = #externalMacro(module: "MacroDefinition", type: "WrapAllProperties")

@propertyWrapper
struct Wrapper<T> {
  init(wrappedValue: T) {
    print("initializing wrapper with \(wrappedValue)")
    self.wrappedValue = wrappedValue
  }
  var wrappedValue: T {
    willSet { print("setting \(newValue)") }
  }
}

protocol P {
  func reading<EnclosingSelf, Value>(from keyPath: KeyPath<EnclosingSelf, Value>)
  func writing<EnclosingSelf, Value>(to keyPath: KeyPath<EnclosingSelf, Value>)
}

extension P {
  func reading<EnclosingSelf, Value>(from keyPath: KeyPath<EnclosingSelf, Value>) {
    print("reading from key-path")
  }

  func writing<EnclosingSelf, Value>(to keyPath: KeyPath<EnclosingSelf, Value>) {
    print("writing to key-path")
  }
}

@propertyWrapper
struct EnclosingSelfWrapper<Value> {
  private var stored: Value

  init(wrappedValue: Value) { self.stored = wrappedValue }

  @available(*, unavailable)
  var wrappedValue: Value {
    get { fatalError() }
    set { fatalError() }
  }

  static subscript<EnclosingSelf: P>(
    _enclosingInstance observed: EnclosingSelf,
    wrapped wrappedKeyPath: ReferenceWritableKeyPath<EnclosingSelf, Value>,
    storage storageKeyPath: ReferenceWritableKeyPath<EnclosingSelf, Self>
  ) -> Value {
    get {
      observed.reading(from: wrappedKeyPath)
      return observed[keyPath: storageKeyPath].stored
    }
    set {
      observed.writing(to: wrappedKeyPath)
      observed[keyPath: storageKeyPath].stored = newValue

    }
  }
}

@wrapAllProperties
struct S {
  var x: Int
  var y: Int
}

// CHECK: initializing wrapper with 0
// CHECK: initializing wrapper with 1
var s = S(x: 0, y: 1)

// CHECK: setting 10
s.x = 10

// CHECK: setting 100
s.y = 100


@wrapAllProperties
class C: P {
  var x: Int = 0
  var y: Int = 0
}

var c = C()
// CHECK: reading from key-path
_ = c.x
// CHECK: reading from key-path
_ = c.y

// CHECK: writing to key-path
c.x = 10
// CHECK: writing to key-path
c.y = 100
