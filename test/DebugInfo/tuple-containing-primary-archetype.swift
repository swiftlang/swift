// RUN: %target-swift-frontend -primary-file %s -emit-ir -O -g -o - | %FileCheck %s

// Check that the tuple sizes aren't mixed up between the two functions

open class C {
}

public protocol P {
  associatedtype S
}

extension Array {
  public func fill<B: P>(b: B) where Element == B.S? {
    for (_, _) in enumerated() {
    }
  }
  public func fill<B: P>(a: UnsafeMutablePointer<B?>) where Element == B.S?, B.S: C {
    for (i, _) in enumerated() {
      a[i] = nil
    }
  }
}

// CHECK-DAG: !DILocalVariable(name: "$element", {{.*}} type: ![[DT1:[0-9]+]])
// CHECK-DAG: ![[DT1]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[CT1:[0-9]+]])
// CHECK-DAG: ![[CT1]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSi6offset_1S4main1PPQyd__Sg7elementtD", {{.*}} size: {{64|32}}, runtimeLang: DW_LANG_Swift, identifier: "$sSi6offset_1S4main1PPQyd__Sg7elementtD")

// CHECK-DAG: !DILocalVariable(name: "$element", {{.*}} type: ![[DT2:[0-9]+]])
// CHECK-DAG: ![[DT2]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[CT2:[0-9]+]])
// CHECK-DAG: ![[CT2]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSi6offset_1S4main1PPQyd__Sg7elementtD", {{.*}} size: 128, runtimeLang: DW_LANG_Swift, identifier: "$sSi6offset_1S4main1PPQyd__Sg7elementtD")

