// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -Xllvm -sil-inline-generics=true %s -O -emit-sil -g -o - -emit-ir | %FileCheck %s
public protocol P {
  associatedtype DT1
  func getDT() -> DT1
}
 
@inline(__always)
func foo1<T:P>(_ t: T, _ dt: T.DT1) -> T.DT1 {
  var dttmp: T.DT1 = dt
  return dttmp
}

// CHECK: define {{.*}}@_T04main4foo2yxAA1PRzlF
public func foo2<S:P>(_ s: S) {
  // CHECK: call void @llvm.dbg.value(metadata %swift.type* %S.DT1, i64 0,
  // CHECK-SAME:                     metadata ![[META:[0-9]+]]
  foo1(s, s.getDT())
  // T.DT1 should get substituted with S.DT1.
  // CHECK: ![[META]] = !DILocalVariable(name: "$swift.type.S.DT1"
}
