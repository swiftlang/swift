// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %s -emit-ir | %FileCheck %s

public struct BigStruct {
  var i0 : Int32 = 0
  var i1 : Int32 = 1
  var i2 : Int32 = 2
  var i3 : Int32 = 3
  var i4 : Int32 = 4
  var i5 : Int32 = 5
  var i6 : Int32 = 6
  var i7 : Int32 = 7
  var i8 : Int32 = 8
}

public func f1_returns_BigType(_ x: BigStruct) -> BigStruct {
  return x
}

public func f2_returns_f1() -> (_ x: BigStruct) -> BigStruct {
  return f1_returns_BigType
}

public func f3_uses_f2() {
  let x = BigStruct()
  let useOfF2 = f2_returns_f1()
  let _ = useOfF2(x)
}

// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T022big_types_corner_cases10f3_uses_f2yyF()
// CHECK: call swiftcc void @_T022big_types_corner_cases9BigStructVACycfC(%T22big_types_corner_cases9BigStructV* noalias nocapture sret
// CHECK: call swiftcc { i8*, %swift.refcounted* } @_T022big_types_corner_cases13f2_returns_f1AA9BigStructVADcyF()
// CHECK: call swiftcc void %16(%T22big_types_corner_cases9BigStructV* noalias nocapture sret %call.aggresult1, %T22big_types_corner_cases9BigStructV* noalias nocapture dereferenceable
// CHECK: ret void
