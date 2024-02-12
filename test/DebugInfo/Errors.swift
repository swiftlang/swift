// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
public enum E : Error { case Err }

// Function throws.
public func throwError() throws { throw E.Err }
// CHECK: !DISubprogram(name: "throwError", {{.*}}thrownTypes: ![[THROWN:.*]])
// CHECK-DAG: ![[THROWN]] = !{![[ERROR:[0-9]+]]}
// CHECK-DAG: ![[ERROR]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Error"


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

// Function with typed throws.
// CHECK: !DISubprogram(name: "genericRethrow", {{.*}}thrownTypes: ![[GENERIC_THROWN:.*]])
// CHECK: ![[GENERIC_THROWN]] = !{![[GENERIC_THROWN_INNER:.*]]}
// CHECK: ![[GENERIC_THROWN_INNER]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$sxD", {{.*}}, elements: ![[GENERIC_THROWN_ELEMENTS:.*]], runtimeLang: DW_LANG_Swift)
// CHECK: ![[GENERIC_THROWN_ELEMENTS]] = !{![[GENERIC_THROWN_ELEMENTS_INNER:.*]]}
// CHECK: ![[GENERIC_THROWN_ELEMENTS_INNER]] = !DIDerivedType(tag: DW_TAG_inheritance, {{.*}}baseType: ![[GENERIC_THROWN_BASE:.*]], extraData: {{.*}})
// CHECK: ![[GENERIC_THROWN_BASE]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$ss5Error_pmD", size: {{.*}}, flags: DIFlagArtificial, runtimeLang: DW_LANG_Swift, identifier: "$ss5Error_pmD")
public func genericRethrow<E: Error>(fn: () throws(E) -> Void) throws(E) {
  try fn()
}

// Negative tests.
// CHECK: !DISubprogram(name: "returnThrowing",
// CHECK-NOT:           thrownTypes:
public func returnThrowing() -> (() throws -> ()) { return throwError }
// CHECK: !DISubprogram(name: "takesThrowing",
// CHECK-NOT:           thrownTypes:
public func takesThrowing(fn : (() throws -> ())) {}
