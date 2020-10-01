// RUN: %target-swift-frontend -emit-sil -Onone %s | %FileCheck %s

// This file contains test cases that shows that we can properly conditional
// compile out code in -Onone contexts using transparent. It is important to
// note that all test cases here should have _BOTH_ generic and concrete
// implementations. Users should be able to depend on this in simple transparent
// cases.
//
// The first check, makes sure our silgen codegen is as we expect it. The second
// makes sure we optimize is as expected.

enum MyEnum {
case first
case second
}

@_cdecl("cFuncOriginal")
@inline(never)
func cFuncOriginal() -> () {}

@_cdecl("cFuncRefinement")
@inline(never)
func cFuncRefinement() -> () {}

class Klass {
  final var value: MyEnum = .first
}

protocol OriginalProtocol {
  var value: Optional<Klass> { get }
}

extension OriginalProtocol {
  @_transparent
  var value: Optional<Klass> {
    cFuncOriginal()
    return nil
  }
}

protocol RefinementProtocol : OriginalProtocol {
  var klass: Klass { get }
  var value: Optional<Klass> { get }
}

extension RefinementProtocol {
  @_transparent
  var value: Optional<Klass> {
    cFuncRefinement()
    return klass
  }
}

struct OriginalProtocolImpl {}
extension OriginalProtocolImpl : OriginalProtocol {}

struct RefinementProtocolImpl {
  private var _klass: Klass = Klass()
  @_transparent var klass: Klass { return _klass }
}
extension RefinementProtocolImpl : RefinementProtocol {}

@_transparent
func transparentAddressCallee<T : OriginalProtocol>(_ t: T) -> MyEnum {
  if let x = t.value {
    return x.value
  }
  return .second
}

// CHECK-LABEL: sil hidden @$s49mandatory_conditional_compile_out_using_optionals24testOriginalProtocolImplAA6MyEnumOyF : $@convention(thin) () -> MyEnum {
// CHECK-NOT: function_ref @$s49mandatory_conditional_compile_out_using_optionals15cFuncRefinementyyF :
// CHECK: [[FUNCTION_REF:%.*]] = function_ref @$s49mandatory_conditional_compile_out_using_optionals13cFuncOriginalyyF :
// CHECK-NEXT: apply [[FUNCTION_REF]]()
// CHECK-NOT: function_ref @$s49mandatory_conditional_compile_out_using_optionals15cFuncRefinementyyF :
// CHECK: } // end sil function '$s49mandatory_conditional_compile_out_using_optionals24testOriginalProtocolImplAA6MyEnumOyF'
func testOriginalProtocolImpl() -> MyEnum {
  let x = OriginalProtocolImpl()
  return transparentAddressCallee(x)
}

// CHECK-LABEL: sil hidden @$s49mandatory_conditional_compile_out_using_optionals26testRefinementProtocolImplAA6MyEnumOyF : $@convention(thin) () -> MyEnum {
// CHECK-NOT: function_ref @$s49mandatory_conditional_compile_out_using_optionals13cFuncOriginalyyF :
// CHECK: [[FUNCTION_REF:%.*]] = function_ref @$s49mandatory_conditional_compile_out_using_optionals15cFuncRefinementyyF :
// CHECK-NEXT: apply [[FUNCTION_REF]]()
// CHECK-NOT: function_ref @$s49mandatory_conditional_compile_out_using_optionals13cFuncOriginalyyF :
// CHECK: } // end sil function '$s49mandatory_conditional_compile_out_using_optionals26testRefinementProtocolImplAA6MyEnumOyF'
func testRefinementProtocolImpl() -> MyEnum {
  let x = RefinementProtocolImpl()
  return transparentAddressCallee(x)
}

@_transparent
func transparentObjectCallee<T : OriginalProtocol>(_ t: T) -> MyEnum where T : AnyObject {
  if let x = t.value {
    return x.value
  }
  return .second
}

class OriginalProtocolImplKlass {
}
extension OriginalProtocolImplKlass : OriginalProtocol {
}

class RefinementProtocolImplKlass {
}
extension RefinementProtocolImplKlass : RefinementProtocol {
  var klass: Klass {
    return Klass()
  }
}

// CHECK-LABEL: sil hidden @$s49mandatory_conditional_compile_out_using_optionals29testOriginalProtocolImplKlassAA6MyEnumOyF : $@convention(thin) () -> MyEnum {
// CHECK-NOT: function_ref @$s49mandatory_conditional_compile_out_using_optionals15cFuncRefinementyyF :
// CHECK: [[FUNCTION_REF:%.*]] = function_ref @$s49mandatory_conditional_compile_out_using_optionals13cFuncOriginalyyF :
// CHECK-NEXT: apply [[FUNCTION_REF]]()
// CHECK-NOT: function_ref @$s49mandatory_conditional_compile_out_using_optionals15cFuncRefinementyyF :
// CHECK: } // end sil function '$s49mandatory_conditional_compile_out_using_optionals29testOriginalProtocolImplKlassAA6MyEnumOyF'
func testOriginalProtocolImplKlass() -> MyEnum {
  let x = OriginalProtocolImplKlass()
  return transparentObjectCallee(x)
}

// CHECK-LABEL: sil hidden @$s49mandatory_conditional_compile_out_using_optionals31testRefinementProtocolImplKlassAA6MyEnumOyF : $@convention(thin) () -> MyEnum {
// CHECK-NOT: function_ref @$s49mandatory_conditional_compile_out_using_optionals13cFuncOriginalyyF :
// CHECK: [[FUNCTION_REF:%.*]] = function_ref @$s49mandatory_conditional_compile_out_using_optionals15cFuncRefinementyyF :
// CHECK-NEXT: apply [[FUNCTION_REF]]()
// CHECK-NOT: function_ref @$s49mandatory_conditional_compile_out_using_optionals13cFuncOriginalyyF :
// CHECK: } // end sil function '$s49mandatory_conditional_compile_out_using_optionals31testRefinementProtocolImplKlassAA6MyEnumOyF'
func testRefinementProtocolImplKlass() -> MyEnum {
  let x = RefinementProtocolImplKlass()
  return transparentObjectCallee(x)
}
