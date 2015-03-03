// RUN: %target-swift-frontend -O -emit-sil %s | FileCheck %s
// We want to check two things here:
// - Correctness
// - That certain "is" checks are eliminated based on static analysis at compile-time
//
// In ideal world, all those testNN functions should be simplified down to a single basic block 
// which returns either true or false, i.e. all type checks should folded statitcally.

import Foundation

class ObjCX : NSObject {

}

struct CX: _ObjectiveCBridgeable {
   static func _isBridgedToObjectiveC() -> Bool {
	return true
   }   
   
   static func _getObjectiveCType() -> Any.Type {
      return String.self
   }
   
   func _bridgeToObjectiveC() -> ObjCX {
	return ObjCX()   
   }
   
   static func _forceBridgeFromObjectiveC(source: ObjCX, inout result: CX?) {
   
   }
   
   static func _conditionallyBridgeFromObjectiveC(source: ObjCX, inout result: CX?) -> Bool {
	return false
   }

}


protocol P {}

protocol Q: P {}

class A:P {}

class X {}

class B:P {}

private struct S:P {}

struct T:Q {}

private struct U {}

public protocol CP1: class {}

public protocol CP2: class {}

// Class D implements only one of class protocols
// and it cannot be extended elsewhere as it is private
private class D: CP1 {}

// Class E implements both class protocols at once
class E: CP1, CP2 {}

func cast0<T>(a:T) -> Bool {
  // Succeeds if T is A
  return A().dynamicType is T.Type
}


func cast1<T>(a:T) -> Bool {
  // Succeeds if T is A
  return (A() as AnyObject).dynamicType is T.Type
}

func cast2<T>(a:T) -> Bool {
  // Succeeds if T is A
  let ao:AnyObject = A() as AnyObject
  return ao.dynamicType is T.Type
}


func cast3(p:AnyObject) -> Bool {
  // Always fails
  return p.dynamicType is AnyObject.Protocol
}

func cast4(p:AnyObject) -> Bool {
  return p.dynamicType is A.Type
}

func cast5(t:AnyObject.Type) -> Bool {
  // Succeeds if t is B.self
  return t is B.Type
}

func cast6<T>(t:T) -> Bool {
  // Always fails
  return AnyObject.self is T.Type
}

func cast7<T>(t:T.Type) -> Bool {
  // Succeeds if t is AnyObject.self
  return t is AnyObject.Protocol
}


func cast8<T>(a:T) -> Bool {
  // Succeeds if T is A
  return (A() as P).dynamicType is T.Type
}

func cast9<T>(a:T) -> Bool {
  // Succeeds if T is A
  let ao:P = A() as P
  return ao.dynamicType is T.Type
}


func cast10(p:P) -> Bool {
  return p.dynamicType is P.Protocol
}

func cast11(p:P) -> Bool {
  // Succeeds if p is of type A
  return p.dynamicType is A.Type
}

func cast12(t:P.Type) -> Bool {
  return t is B.Type
}


func cast13<T>(t:T) -> Bool {
  // Succeeds if T is P
  return P.self is T.Type
}


func cast14<T>(t:T.Type) -> Bool {
  // Succeeds if p is P.self
  return t is P.Protocol
}

func cast15<T>(t:T) -> Bool {
  // Succeeds if T is P
  return P.self is T.Type
}

func cast16<T>(t:T) -> Bool {
  // Succeeds if T is P
  return T.self is P.Protocol
}


func cast17<T>(t:T) -> Bool {
  // Succeeds if T is AnyObject
  return AnyObject.self is T.Type
}

func cast18<T>(t:T) -> Bool {
  // Succeeds if T is AnyObject
  return T.self is AnyObject.Protocol
}

func cast20<T>(t:T) -> Bool {
  // Succeeds if T is P
  return T.self is P.Type
}

func cast21<T>(t:T) -> Bool {
  // Succeeds if T is P
  return T.self is P.Protocol
}

func cast22(existential: P.Type) -> Bool {
  // Succeeds if existential is S.self
  return existential is S.Type
}

func cast23(concrete: P.Protocol) -> Bool {
  // Always fails
  return concrete is S.Type
}


func cast24(existential: P.Type) -> Bool {
  // Succeeds if existential is Q.self
  return existential is Q.Type
}


func cast25(concrete: P.Protocol) -> Bool {
  // Always fails, because P != Q
  return concrete is Q.Type
}

func cast26(existential: Q.Type) -> Bool {
  // Succeeds always, because Q:P
  return existential is P.Type
}

func cast27(existential: CP1.Type) -> Bool {
  // Fails always, because existential is a class
  // and it cannot be struct
  return existential is S.Type
}

func cast28(existential: CP1.Type) -> Bool {
  // Succeds if existential conforms to CP1 and CP2
  return existential is CP2.Type
}

// Check casts to types which are _ObjectiveCBridgeable
func cast29(o: AnyObject) -> Bool {
  return o is CX
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding5test0FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test0() -> Bool {
  return cast0(A())
}


// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding5test1FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test1() -> Bool {
  return cast1(A())
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding5test2FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test2() -> Bool {
  return cast2(A())
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding5test3FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, 0
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test3() -> Bool {
  return cast3(A())
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding5test4FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test4() -> Bool {
  return cast4(A())
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding7test5_1FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test5_1() -> Bool {
    return cast5(B.self)
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding7test5_2FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, 0
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test5_2() -> Bool {
    return cast5(AnyObject.self)
}


// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding7test6_1FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, 0
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test6_1() -> Bool {
    return cast6(B.self)
}


// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding7test6_2FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, 0
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test6_2() -> Bool {
    return cast6(AnyObject.self)
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding7test7_1FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, 0
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test7_1() -> Bool {
    return cast7(B.self)
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding7test7_2FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test7_2() -> Bool {
    return cast7(AnyObject.self)
}


// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding5test8FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test8() -> Bool {
  return cast8(A())
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding5test9FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test9() -> Bool {
  return cast9(A())
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding6test10FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, 0
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test10() -> Bool {
  return cast10(A())
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding6test11FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test11() -> Bool {
  return cast11(A())
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test12_1FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, 0
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test12_1() -> Bool {
    return cast12(A.self)
}


// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test12_2FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, 0
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test12_2() -> Bool {
    return cast12(P.self)
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test13_1FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, 0
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test13_1() -> Bool {
    return cast13(A.self)
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test13_2FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, 0
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test13_2() -> Bool {
    return cast13(P.self)
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test13_3FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test13_3() -> Bool {
    return cast13(A() as P)
}


// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test14_1FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, 0
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test14_1() -> Bool {
    return cast14(A.self)
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test14_2FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test14_2() -> Bool {
    return cast14(P.self)
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test15_1FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, 0
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test15_1() -> Bool {
    return cast15(A())
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test15_2FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test15_2() -> Bool {
    return cast15(A() as P)
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test16_1FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, 0
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test16_1() -> Bool {
    return cast16(A())
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test16_2FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test16_2() -> Bool {
    return cast16(A() as P)
}


// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test17_1FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, 0
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test17_1() -> Bool {
    return cast17(A())
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test17_2FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test17_2() -> Bool {
    return cast17(A() as AnyObject)
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test18_1FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, 0
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test18_1() -> Bool {
    return cast18(A())
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test18_2FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test18_2() -> Bool {
    return cast18(A() as AnyObject)
}


// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding6test19FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test19() -> Bool {
    let t: Any.Type = (1 as Any).dynamicType
    return t is Int.Type
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test20_1FT_Sb : $@thin () -> Bool
@inline(never)
func test20_1() -> Bool {
    return cast20(S.self)
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test20_2FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, 0
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test20_2() -> Bool {
    return cast20(U())
}


// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test21_1FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, 0
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test21_1() -> Bool {
    return cast21(S.self)
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test21_2FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test21_2() -> Bool {
    return cast21(A() as P)
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test22_1FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, 0
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test22_1() -> Bool {
    return cast22(T.self)
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test22_2FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test22_2() -> Bool {
    return cast22(S.self)
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding6test23FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, 0
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test23() -> Bool {
    return cast23(P.self)
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test24_1FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test24_1() -> Bool {
    return cast24(T.self)
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test24_2FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, 0
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test24_2() -> Bool {
    return cast24(S.self)
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test24_3FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test24_3() -> Bool {
    return cast24(Q.self)
}


// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding6test25FT_Sb : $@thin () -> Bool
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, 0
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test25() -> Bool {
    return cast25(P.self)
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding6test26FT_Sb
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test26() -> Bool {
    return cast26(T.self)
}


// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding6test27FT_Sb
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, 0
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test27() -> Bool {
    return cast27(D.self)
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test28_1FT_Sb
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, 0
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test28_1() -> Bool {
    return cast28(D.self)
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding8test28_2FT_Sb
// CHECK: bb0
// CHECK-NEXT: %0 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT: %1 = struct $Bool
// CHECK-NEXT: return %1 
@inline(never)
func test28_2() -> Bool {
    return cast28(E.self)
}

// CHECK-LABEL: sil hidden [noinline] @_TF12cast_folding6test29FT_Sb
// CHECK: bb0
// Check that cast is not elmiminated even though cast29 is a conversion
// from a class to struct, because it casts to a struct implementing
// the _BridgedToObjectiveC protocol
// CHECK: checked_cast
// CHECK: return 
@inline(never)
func test29() -> Bool {
  return cast29(NSNumber(integer:1))
}

println("test0=\(test0())")

println("test1=\(test1())")

println("test2=\(test2())")

println("test3=\(test3())")

println("test4=\(test4())")

println("test5_1=\(test5_1())")

println("test5_2=\(test5_2())")

println("test6_1=\(test6_1())")

println("test6_2=\(test6_2())")

println("test7_1=\(test7_1())")

println("test7_2=\(test7_2())")


println("test8=\(test8())")

println("test9=\(test9())")

println("test10=\(test10())")

println("test11=\(test11())")

println("test12_1=\(test12_1())")

println("test12_2=\(test12_2())")

println("test13_1=\(test13_1())")

println("test13_2=\(test13_2())")

println("test13_3=\(test13_3())")

println("test14_1=\(test14_1())")

println("test14_2=\(test14_2())")

println("test15_1=\(test15_1())")

println("test15_2=\(test15_2())")

println("test16_1=\(test16_1())")

println("test16_2=\(test16_2())")


println("test17_1=\(test17_1())")

println("test17_2=\(test17_2())")

println("test18_1=\(test18_1())")

println("test18_2=\(test18_2())")

println("test19=\(test19())")

println("test20_1=\(test20_1())")

println("test20_2=\(test20_2())")

println("test21_1=\(test21_1())")

println("test21_2=\(test21_2())")

println("test22_1=\(test22_1())")

println("test22_2=\(test22_2())")

println("test23=\(test23())")

println("test24_1=\(test24_1())")

println("test24_2=\(test24_2())")

println("test24_3=\(test24_3())")

println("test25=\(test25())")

println("test26=\(test26())")

println("test27=\(test27())")

println("test28_1=\(test28_1())")

println("test28_2=\(test28_2())")

println("test29=\(test29())")
