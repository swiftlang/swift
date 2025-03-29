// RUN: %target-swift-frontend -g -emit-ir %s | %FileCheck %s

public protocol P {
  associatedtype AT;
}

// A generic declcontext cannot be shared, so we expect a
// forward-declared type.
// CHECK-GEN: ![[FWD:.*]] = !DICompositeType({{.*}}, name: "Generic",
// CHECK-GEN-SAME:                           flags: DIFlagFwdDecl
// CHECK-GEN: linkageName: "$s4main7GenericV3sety2ATQzF", scope: ![[FWD]],
public struct Generic<T : P> {
  public func get() -> T.AT? {
    return nil
  }
  public func set(_ t: T.AT) {}
}

// But only one concrete type is expected.
// CHECK: !DISubprogram({{.*}}linkageName: "$s4main8ConcreteV3getSiSgyF",
// CHECK-SAME:          scope: ![[CONC:[0-9]+]],
// CHECK: ![[CONC]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Concrete", scope: !{{[0-9]+}}, file: !{{[0-9]+}}, runtimeLang: DW_LANG_Swift, identifier: "$s4main8ConcreteVD")
public struct Concrete {
  public func get() -> Int? {
    return nil
  }
  public func set(_ t: Int) {}
}

public func getConcrete() -> Concrete? { return nil; }
