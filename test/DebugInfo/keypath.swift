// RUN: %target-swift-frontend -g -emit-ir %s | %FileCheck %s

public struct Gen<Value> {
  public private(set) var value: Value
  public func use<Subject>(keyPath: WritableKeyPath<Value, Subject>) {
  }
}

// This used to assert.

// CHECK-DAG: ![[COLLECTION:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Collection", {{.*}}identifier: "$sSl_pD")
// CHECK-DAG: ![[SUBSCRIPT_DECL:[0-9]+]] = !DISubprogram({{.*}}linkageName: "$sSly7ElementQz5IndexQzcipSMRzSHADRQlxxTK", scope: ![[COLLECTION]]
// CHECK-DAG: distinct !DISubprogram({{.*}}linkageName: "$sSly7ElementQz5IndexQzcipSMRzSHADRQlxxTK", scope: ![[COLLECTION]]{{.*}}spFlags: DISPFlagDefinition{{.*}}declaration: ![[SUBSCRIPT_DECL]]
extension Gen where Value : MutableCollection, Value.Index : Hashable {
    public var dontAssert: Int {
        var i = value.startIndex
				use(keyPath: \.[i])
        return 0
    }
}
