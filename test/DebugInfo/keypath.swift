// RUN: %target-swift-frontend -g -emit-ir %s | %FileCheck %s

public struct Gen<Value> {
  public private(set) var value: Value
  public func use<Subject>(keyPath: WritableKeyPath<Value, Subject>) {
  }
}

// This used to assert.

// CHECK: distinct !DISubprogram(linkageName: "keypath_set", {{.*}} flags: DIFlagArtificial
extension Gen where Value : MutableCollection, Value.Index : Hashable {
    public var dontAssert: Int {
        var i = value.startIndex
				use(keyPath: \.[i])
        return 0
    }
}
