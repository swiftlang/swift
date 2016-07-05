// REQUIRES: rdar27179298
// RUN: %target-swift-frontend -g -emit-ir %s | FileCheck %s

public protocol P {
  associatedtype AT;
}

// A generic declcontext cannot be shared, so we expect a
// forward-declared type.
// CHECK-GEN: ![[FWD:.*]] = !DICompositeType({{.*}}, name: "Generic",
// CHECK-GEN-SAME:                           flags: DIFlagFwdDecl
// CHECK-GEN: linkageName: "_TFV4main7Generic3setfwx2ATT_", scope: ![[FWD]],
public struct Generic<T : P> {
  public func get() -> T.AT? {
    return nil
  }
  public func set(_ t: T.AT) {}
}

// But only one concrete type is expected.
// CHECK: !DISubprogram({{.*}}linkageName: "_TFV4main8Concrete3getfT_GSqSi_",
// CHECK-SAME:          scope: ![[CONC:[0-9]+]],
// CHECK: ![[CONC]] = !DICompositeType({{.*}}, name: "Concrete",
// CHECK-NOT: DIFlagFwdDecl
// CHECK-NEXT: =
public struct Concrete {
  public func get() -> Int? {
    return nil
  }
  public func set(_ t: Int) {}
}

public func getConcrete() -> Concrete? { return nil; }
