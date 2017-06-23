// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -enable-large-loadable-types %s -emit-ir | %FileCheck %s

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

func takeClosure(execute block: () -> Void) {
}

class OptionalInoutFuncType {
  private var lp :  BigStruct?
  private var _handler : ((BigStruct?, Error?) -> ())?

  func execute(_ error: Error?) {
    var p : BigStruct?
    var handler: ((BigStruct?, Error?) -> ())?
    
    takeClosure {
      p = self.lp
      handler = self._handler
      self._handler = nil
    }

    handler?(p, error)
  }
}

// CHECK-LABEL: define{{( protected)?}} internal swiftcc void @_T022big_types_corner_cases21OptionalInoutFuncTypeC7executeys5Error_pSgFyycfU_(%T22big_types_corner_cases9BigStructVSg* nocapture dereferenceable({{.*}}), %T22big_types_corner_cases21OptionalInoutFuncTypeC*, %T22big_types_corner_cases9BigStructVSgs5Error_pSgIxcx_Sg* nocapture dereferenceable({{.*}})
// CHECK: call void @_T0SqWy
// CHECK: call void @_T0SqWe
// CHECK: ret void

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

public func f4_tuple_use_of_f2() {
  let x = BigStruct()
  let tupleWithFunc = (f2_returns_f1(), x)
  let useOfF2 = tupleWithFunc.0
  let _ = useOfF2(x)
}

// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T022big_types_corner_cases18f4_tuple_use_of_f2yyF()
// CHECK: [[TUPLE:%.*]] = call swiftcc { i8*, %swift.refcounted* } @_T022big_types_corner_cases13f2_returns_f1AA9BigStructVADcyF()
// CHECK: [[TUPLE_EXTRACT:%.*]] = extractvalue { i8*, %swift.refcounted* } [[TUPLE]], 0
// CHECK: [[CAST_EXTRACT:%.*]] = bitcast i8* [[TUPLE_EXTRACT]] to void (%T22big_types_corner_cases9BigStructV*, %T22big_types_corner_cases9BigStructV*, %swift.refcounted*)*
// CHECK:  call swiftcc void [[CAST_EXTRACT]](%T22big_types_corner_cases9BigStructV* noalias nocapture sret %call.aggresult1, %T22big_types_corner_cases9BigStructV* noalias nocapture dereferenceable
// CHECK: ret void

public class BigClass {
  public init() {
  }

  public var optVar: ((BigStruct)-> Void)? = nil
    
  func useBigStruct(bigStruct: BigStruct) {
    optVar!(bigStruct)
  }
}

// CHECK-LABEL define{{( protected)?}} hidden swiftcc void @_T022big_types_corner_cases8BigClassC03useE6StructyAA0eH0V0aH0_tF(%T22big_types_corner_cases9BigStructV* noalias nocapture dereferenceable({{.*}}), %T22big_types_corner_cases8BigClassC* swiftself) #0 {
// CHECK: getelementptr inbounds %T22big_types_corner_cases8BigClassC, %T22big_types_corner_cases8BigClassC*
// CHECK: call void @_T0SqWy
// CHECK: [[BITCAST:%.*]] = bitcast i8* {{.*}} to void (%T22big_types_corner_cases9BigStructV*, %swift.refcounted*)*
// CHECK: call swiftcc void [[BITCAST]](%T22big_types_corner_cases9BigStructV* noalias nocapture dereferenceable({{.*}}) %0, %swift.refcounted* swiftself 
// CHECK: ret void

public struct MyStruct {
  public let a: Int
  public let b: String?
 }

typealias UploadFunction = ((MyStruct, Int?) -> Void) -> Void
func takesUploader(_ u: UploadFunction) { }

class Foo {
  func blam() {
    takesUploader(self.myMethod) // crash compiling this
  }

  func myMethod(_ callback: (MyStruct, Int) -> Void) -> Void { }
}

// CHECK-LABEL: define{{( protected)?}} linkonce_odr hidden swiftcc { i8*, %swift.refcounted* } @_T022big_types_corner_cases3FooC8myMethodyyAA8MyStructV_SitcFTc(%T22big_types_corner_cases3FooC*)
// CHECK: getelementptr inbounds %T22big_types_corner_cases3FooC, %T22big_types_corner_cases3FooC*
// CHECK: getelementptr inbounds void (i8*, %swift.refcounted*, %T22big_types_corner_cases3FooC*)*, void (i8*, %swift.refcounted*, %T22big_types_corner_cases3FooC*)**
// CHECK: call noalias %swift.refcounted* @swift_rt_swift_allocObject(%swift.type* getelementptr inbounds (%swift.full_boxmetadata, %swift.full_boxmetadata*
// CHECK: ret { i8*, %swift.refcounted* }

public enum LargeEnum {
  public enum InnerEnum {
    case simple(Int64)
    case hard(Int64, String?)
  }
  case Empty1
  case Empty2
  case Full(InnerEnum)
}

public func enumCallee(_ x: LargeEnum) {
  switch x {
    case .Full(let inner): print(inner)
    case .Empty1: break
    case .Empty2: break
  }
}
// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T022big_types_corner_cases10enumCalleeyAA9LargeEnumOF(%T22big_types_corner_cases9LargeEnumO* noalias nocapture dereferenceable(34)) #0 {
// CHECK: alloca %T22big_types_corner_cases9LargeEnumO05InnerF0O
// CHECK: alloca %T22big_types_corner_cases9LargeEnumO
// CHECK: call void @llvm.memcpy.p0i8.p0i8.i64
// CHECK: call void @llvm.memcpy.p0i8.p0i8.i64
// CHECK: call %swift.type* @_T0ypMa()
// CHECK: ret void
