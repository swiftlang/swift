// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name preconcurrency -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | %FileCheck %s

class C {
  @preconcurrency func f(_: Sendable) { }
  @preconcurrency static func g(_: Sendable) { }
}

@preconcurrency func f(_: Sendable) { }

// CHECK-LABEL: sil hidden [ossa] @$s14preconcurrency28testModuleMethodWithSendable3anyyyp_tF
func testModuleMethodWithSendable(any: Any) {
  // CHECK: function_ref @$s14preconcurrency1fyyypF : $@convention(thin) (@in_guaranteed any Sendable) -> ()
  let _ = f

  // CHECK: function_ref @$s14preconcurrency1fyyypF : $@convention(thin) (@in_guaranteed any Sendable) -> ()
  let _ = preconcurrency.f
  f(any)
  preconcurrency.f(any)
}

// CHECK-LABEL: sil hidden [ossa] @$s14preconcurrency30testInstanceMethodWithSendable1c3anyyAA1CC_yptF : $@convention(thin) (@guaranteed C, @in_guaranteed Any) -> () {
func testInstanceMethodWithSendable(c: C, any: Any) {
  // CHECK-LABEL: sil private [ossa] @$s14preconcurrency30testInstanceMethodWithSendable1c3anyyAA1CC_yptFyypcAFcfu_yypcfu0_
  // CHECK: class_method %1 : $C, #C.f : (C) -> (any Sendable) -> ()
  let _ = c.f
  let _ = C.f
  let _ = C.g
  c.f(any)
}

class Request {
  @preconcurrency let identifier: (any Sendable)? = nil
}

func test(from request: Request) {
  // Make sure we don't assert in CSApply when adjusting 'any Sendable' -> 'Any'
  // for preconcurrency
  let _ = request.identifier
}

@propertyWrapper
struct Wrapper<T> {
  var wrappedValue: T

  init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }
  init(projectedValue: Self) {
    self = projectedValue
  }
  var projectedValue: Self { self }
}

// rdar://140212823 - Make sure we can handle the Sendable promotion of `y` in
// the curry thunk.
@preconcurrency func hasWrapperAndPreconcurrency(@Wrapper _ x: Int, _ y: Sendable) {}
struct HasWrapperAndPreconcurrency {
  @preconcurrency func instanceMethod(@Wrapper _ x: Int, _ y: Sendable) {}
  @preconcurrency static func staticMethod(@Wrapper _ x: Int, _ y: Sendable) {}
}
func testPropertyWrapperPreconcurrencyThunk(_ x: HasWrapperAndPreconcurrency) {
  let fn = (hasWrapperAndPreconcurrency)
  fn(0, C())

  _ = (HasWrapperAndPreconcurrency.staticMethod)
  _ = (type(of: x).staticMethod)
  _ = (x.instanceMethod)
  _ = (type(of: x).instanceMethod)
}
