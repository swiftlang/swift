// REQUIRES: swift_swift_parser, executable_test, asserts, concurrency, concurrency_runtime
// REQUIRES: swift_feature_PreambleMacros
// REQUIRES: swift_feature_CoroutineAccessors
// REQUIRES: swift_feature_BorrowAndMutateAccessors
// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath -swift-version 5

// Diagnostics testing
// RUN: %target-typecheck-verify-swift -swift-version 5 -enable-experimental-feature PreambleMacros -enable-experimental-feature BorrowAndMutateAccessors -enable-experimental-feature CoroutineAccessors -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser -DTEST_DIAGNOSTICS

// Execution testing
// RUN: %target-build-swift -swift-version 5 -g -enable-experimental-feature PreambleMacros -enable-experimental-feature BorrowAndMutateAccessors -enable-experimental-feature CoroutineAccessors -load-plugin-library %t/%target-library-name(MacroDefinition) %s -o %t/main -module-name MacroUser
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

@attached(body)
macro Remote() = #externalMacro(module: "MacroDefinition", type: "RemoteBodyMacro")

@attached(body)
macro Print() = #externalMacro(module: "MacroDefinition", type: "PrintBodyMacro")

@attached(preamble)
macro Traced() = #externalMacro(module: "MacroDefinition", type: "TracedPreambleMacro")

@attached(preamble, names: named(logger))
macro Logged() = #externalMacro(module: "MacroDefinition", type: "LoggerMacro")

protocol ConjureRemoteValue {
  static func conjureValue() -> Self
}

extension String: ConjureRemoteValue {
  static func conjureValue() -> String { "" }
}

struct Logger {
  func log(entering function: String) {
    print("Logger entering \(function)")
  }

  func log(_ message: String) {
    print("--- \(message)")
  }

  func log(exiting function: String) {
    print("Logger exiting \(function)")
  }
}

func log(_ message: String) {
  print(message)
}

func remoteCall<Result: ConjureRemoteValue>(function: String, arguments: [String: Any]) async throws -> Result {
  let printedArgs = arguments.keys.sorted().map { key in
    "\(key): \(arguments[key]!)"
  }.joined(separator: ", ")
  print("Remote call \(function)(\(printedArgs))")
  return Result.conjureValue()
}

@Remote
func f(a: Int, b: String) async throws -> String

@Traced
func doubleTheValue(value: Int) -> Int {
  return value * 2
}

@Logged
func useLogger() {
  let x = 1
  logger.log("use it")
  print(x)
}

@Remote
@Traced
@Logged
func g(a: Int, b: String) async throws -> String {
  doesNotTypeCheck()
}

#if compiler(>=6.0) && TEST_DIAGNOSTICS
@Remote
func h(a: Int, b: String) async throws -> String {
  does not
  // expected-error@-1{{consecutive statements on a line must be separated by ';'}}
  parse
}
#endif

// CHECK: Entering doubleTheValue(value: 7)
// CHECK-NEXT: Exiting doubleTheValue(value:)
_ = doubleTheValue(value: 7)

// CHECK: Remote call f(a: 5, b: Hello)
print(try await f(a: 5, b: "Hello"))

// CHECK: Entering g(a: 5, b: World)
// CHECK: Logger entering g(a: 5, b: World)
// CHECK: Remote call g(a: 5, b: World)
// CHECK: Logger exiting g(a:b:)
// CHECK: Exiting g(a:b:)
print(try await g(a: 5, b: "World"))

// CHECK: Logger entering useLogger()
// CHECK: --- use it
// CHECK: Logger exiting useLogger()
useLogger()

@Print
var computedVar: Bool {
  print("hello from computedVar")
  return true
}

var computedVarWithExplicitGetter: Bool {
  @Print
  get {
    print("hello from computedVarWithExplicitGetter")
    return true
  }
}

var computedVarWithGetterAndSetter: Bool {
  @Print
  get {
    print("hello from computedVarWithGetterAndSetter")
    return true
  }
  @Print
  set {
    print("Hello from setter: \(newValue)")
  }
}

var storedVarWithWillSetDidSet = false {
  @Print
  willSet {
    print("hello from storedVarWithWillSetDidSet willSet: \(newValue)")
  }
  @Print
  didSet {
    print("hello from storedVarWithWillSetDidSet didSet: \(storedVarWithWillSetDidSet)")
  }
}

struct UniqueBorrow: ~Copyable {
  var x: Int

  var propertyWithYieldingBorrowYieldingMutate: Int {
    @Print
    yielding borrow {
      print("Hello from yielding borrow")
      yield x
    }
    @Print
    yielding mutate {
      print("Hello from yielding mutate")
      yield &x
    }
  }
  
  var propertyWithBorrowMutate: Int {
    @Print
    borrow {
      print("Hello from borrow")
      return x
    }
    @Print
    mutate {
      print("Hello from mutate")
      return &x
    }
  }
}

// CHECK: start body (from macro, computedVar)
// CHECK-NEXT: hello from computedVar
// CHECK-NEXT: end body (from macro, computedVar)
_ = computedVar

// CHECK: start body (from macro, computedVarWithExplicitGetter)
// CHECK-NEXT: hello from computedVarWithExplicitGetter
// CHECK-NEXT: end body (from macro, computedVarWithExplicitGetter)
_ = computedVarWithExplicitGetter

// CHECK: start body (from macro, computedVarWithGetterAndSetter)
// CHECK-NEXT: hello from computedVarWithGetterAndSetter
// CHECK-NEXT: end body (from macro, computedVarWithGetterAndSetter)
_ = computedVarWithGetterAndSetter

// CHECK: start body (from macro, computedVarWithGetterAndSetter)
// CHECK-NEXT: Hello from setter: false
// CHECK-NEXT: end body (from macro, computedVarWithGetterAndSetter)
computedVarWithGetterAndSetter = false

// CHECK: start body (from macro, storedVarWithWillSetDidSet)
// CHECK-NEXT: hello from storedVarWithWillSetDidSet willSet: true
// CHECK-NEXT: end body (from macro, storedVarWithWillSetDidSet)
// CHECK-NEXT: start body (from macro, storedVarWithWillSetDidSet)
// CHECK-NEXT: hello from storedVarWithWillSetDidSet didSet: true
// CHECK-NEXT: end body (from macro, storedVarWithWillSetDidSet)
storedVarWithWillSetDidSet = true

var uniqueBorrow = UniqueBorrow(x: 10)

// CHECK: start body (from macro, UniqueBorrow.propertyWithYieldingBorrowYieldingMutate)
// CHECK-NEXT: Hello from yielding borrow
// CHECK-NEXT: end body (from macro, UniqueBorrow.propertyWithYieldingBorrowYieldingMutate)
_ = uniqueBorrow.propertyWithYieldingBorrowYieldingMutate

// CHECK: start body (from macro, UniqueBorrow.propertyWithYieldingBorrowYieldingMutate)
// CHECK-NEXT: Hello from yielding mutate
// CHECK-NEXT: end body (from macro, UniqueBorrow.propertyWithYieldingBorrowYieldingMutate)
uniqueBorrow.propertyWithYieldingBorrowYieldingMutate = 10

// CHECK: start body (from macro, UniqueBorrow.propertyWithBorrowMutate)
// CHECK-NEXT: Hello from borrow
// CHECK-NEXT: end body (from macro, UniqueBorrow.propertyWithBorrowMutate)
_ = uniqueBorrow.propertyWithBorrowMutate

// CHECK: start body (from macro, UniqueBorrow.propertyWithBorrowMutate)
// CHECK-NEXT: Hello from mutate
// CHECK-NEXT: end body (from macro, UniqueBorrow.propertyWithBorrowMutate)
uniqueBorrow.propertyWithBorrowMutate = 10

#if compiler(>=6.0) && TEST_DIAGNOSTICS
@Print // expected-error {{'body' macro cannot be attached to var ('storedVar')}}
var storedVar: Bool

@Print // expected-error {{'body' macro cannot be attached to var ('storedVarWithDefault')}}
var storedVarWithDefault = false

@Print // expected-error {{'body' macro cannot be attached to var ('storedVarWithWillSetDidSetAttachedMacro')}}
var storedVarWithWillSetDidSetAttachedMacro = false {
  willSet {
    print("hello from storedVarWithWillSetDidSetAttachedMacro willSet")
  }
  didSet {
    print("hello from storedVarWithWillSetDidSetAttachedMacro didSet")
  }
}

@Print // expected-error {{'body' macro cannot be attached to var ('varWithExplicitGetAndAttachedMacro')}}
var varWithExplicitGetAndAttachedMacro: Bool {
  get {
    print("Hello from varWithExplicitGetAndAttachedMacro")
    return true
  }
}

@Print // expected-error {{'body' macro cannot be attached to var ('varWithGetSetAndAttachedMacro'}}
var varWithGetSetAndAttachedMacro: Bool {
  get {
    print("Hello from varWithGetSetAndAttachedMacro")
    return true
  }
  set {
    print("Hello from varWithGetSetAndAttachedMacro setter: \(newValue)")
  }
}

struct InvalidUniqueBorrow: ~Copyable {
  var x: Int

  @Print // expected-error {{'body' macro cannot be attached to property ('propertyWithYieldingBorrowYieldingMutate')}}
  var propertyWithYieldingBorrowYieldingMutate: Int {
    yielding borrow {
      yield x
    }
    yielding mutate {
      yield &x
    }
  }
  
  @Print // expected-error {{'body' macro cannot be attached to property ('propertyWithBorrowMutate'}}
  var propertyWithBorrowMutate: Int {
    borrow {
      return x
    }
    mutate {
      return &x
    }
  }
}
#endif
