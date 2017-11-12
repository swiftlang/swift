// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
public enum E : Error { case Err }

// Function throws.
public func throwError() throws { throw E.Err }
// CHECK: !DISubprogram(name: "throwError", {{.*}}thrownTypes: ![[THROWN:.*]])
// CHECK: ![[THROWN]] = !{![[ERROR:[0-9]+]]}
// CHECK: ![[ERROR]] = !DICompositeType(tag: DW_TAG_structure_type,
// CHECK-SAME:                          name: "Error"


// Function rethrows.
public func rethrow(fn : (() throws -> ())) rethrows { try fn() }
// CHECK: !DISubprogram(name: "rethrow", {{.*}}thrownTypes: ![[THROWN:.*]])

public class C {
    // Initializer throws.
    init() throws { throw E.Err }
    // CHECK: !DISubprogram(name: "init", {{.*}}line: [[@LINE-1]],
    // CHECK-SAME:          thrownTypes: ![[THROWN:.*]])

    // Initializer rethrows.
    init(fn : (() throws -> ())) rethrows {
        // CHECK: !DISubprogram(name: "init", {{.*}}line: [[@LINE-1]],
        // CHECK-SAME:          thrownTypes: ![[THROWN:.*]])
        try fn()
    }
}

// Negative tests.
// CHECK: !DISubprogram(name: "returnThrowing",
// CHECK-NOT:           thrownTypes:
public func returnThrowing() -> (() throws -> ()) { return throwError }
// CHECK: !DISubprogram(name: "takesThrowing",
// CHECK-NOT:           thrownTypes:
public func takesThrowing(fn : (() throws -> ())) {}
