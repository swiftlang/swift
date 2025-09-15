// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -target %target-swift-5.9-abi-triple %s | %FileCheck %s

public class A<each T> {
  public func f(_ action: (repeat each T) -> ()) {}
}

// CHECK-LABEL: sil hidden [ossa] @$s4main5test02fnyySi_SftXE_tF :
// CHECK:       [[CTOR:%.*]] = function_ref @$s4main1ACACyxxQp_QPGycfC :
// CHECK:       [[OBJECT:%.*]] = apply [[CTOR]]<Pack{Int, Float}>
// CHECK:       [[METHOD:%.*]] = class_method [[OBJECT]] : $A<Int, Float>, #A.f : <each T> (A<repeat each T>) -> ((repeat each T) -> ()) -> ()
// CHECK:       apply [[METHOD]]<Pack{Int, Float}>
func test0(fn: (Int, Float) -> ()) {
  A<Int, Float>().f(fn)
}

// CHECK-LABEL: sil hidden [ossa] @$s4main5test1yyF :
// CHECK:       [[CTOR:%.*]] = function_ref @$s4main1ACACyxxQp_QPGycfC :
// CHECK:       [[OBJECT:%.*]] = apply [[CTOR]]<Pack{Int, Float}>
// CHECK:       [[METHOD:%.*]] = class_method [[OBJECT]] : $A<Int, Float>, #A.f : <each T> (A<repeat each T>) -> ((repeat each T) -> ()) -> ()
// CHECK:       apply [[METHOD]]<Pack{Int, Float}>
func test1() {
  A<Int, Float>().f { a, b in }
}

// These are all currently forbidden
#if false
public class B<each T> : A<repeat each T> {
  public override func f(_ action: (repeat each T) -> ()) {
    super.f(action)
  }
}

func test2() {
  B<Int, Float>().f { a, b in }
}

public class C<T, each U> : A<T, repeat each U> {
  public override func f(_ action: (T, repeat each U) -> ()) {
    super.f(action)
  }
}

func test3() {
  C<Int, Float>().f { a, b in }
}

public class D : A<Int, Float> {
  public override func f(_ action: (Int, Float) -> ()) {
    super.f(action)
  }
}

func test4() {
  D().f { a, b in }
}
#endif
