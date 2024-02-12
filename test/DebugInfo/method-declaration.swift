// RUN: %target-swift-frontend -primary-file %s -emit-ir -gdwarf-types -o - | %FileCheck %s

// Verify that we added a declaration for a method.

// CHECK: define{{.*}}foo_static_method{{.*}} !dbg ![[FOO_STATIC_METHOD_DEF_DBG:[0-9]+]]
// CHECK: define{{.*}}foo_method{{.*}} !dbg ![[FOO_METHOD_DEF_DBG:[0-9]+]]
// CHECK: define{{.*}}bar_static_method{{.*}} !dbg ![[BAR_STATIC_METHOD_DEF_DBG:[0-9]+]]
// CHECK: define{{.*}}bar_method{{.*}} !dbg ![[BAR_METHOD_DEF_DBG:[0-9]+]]
// CHECK: define{{.*}}a_function{{.*}} !dbg ![[FUNC_DEF_DBG:[0-9]+]]

// CHECK-DAG: ![[FOO_DBG:[0-9]+]] = !DICompositeType(tag: {{.*}} name: "Foo", {{.*}} identifier:
public struct Foo {
// CHECK-DAG: ![[FOO_STATIC_METHOD_DEF_DBG]] = distinct !DISubprogram(name: "foo_static_method"{{.*}}, scope: ![[FOO_DBG]]{{.*}}DISPFlagDefinition{{.*}}, declaration: ![[FOO_STATIC_METHOD_DECL_DBG:[0-9]+]]
// CHECK-DAG: ![[FOO_STATIC_METHOD_DECL_DBG]] = !DISubprogram(name: "foo_static_method"{{.*}}, scope: ![[FOO_DBG]]
    static func foo_static_method() {}
// CHECK-DAG: ![[FOO_METHOD_DEF_DBG]] = distinct !DISubprogram(name: "foo_method"{{.*}}, scope: ![[FOO_DBG]]{{.*}}DISPFlagDefinition{{.*}}, declaration: ![[FOO_METHOD_DECL_DBG:[0-9]+]]
// CHECK-DAG: ![[FOO_METHOD_DECL_DBG]] = !DISubprogram(name: "foo_method"{{.*}}, scope: ![[FOO_DBG]]
    func foo_method() {}
}

// CHECK-DAG: ![[BAR_DBG:[0-9]+]] = !DICompositeType(tag: {{.*}} name: "Bar", {{.*}} identifier:
public class Bar {
// CHECK-DAG: ![[BAR_STATIC_METHOD_DEF_DBG]] = distinct !DISubprogram(name: "bar_static_method"{{.*}}, scope: ![[BAR_DBG]]{{.*}}DISPFlagDefinition{{.*}}, declaration: ![[BAR_STATIC_METHOD_DECL_DBG:[0-9]+]]
// CHECK-DAG: ![[BAR_STATIC_METHOD_DECL_DBG]] = !DISubprogram(name: "bar_static_method"{{.*}}, scope: ![[BAR_DBG]]
    static func bar_static_method() {}
// CHECK-DAG: ![[BAR_METHOD_DEF_DBG]] = distinct !DISubprogram(name: "bar_method"{{.*}}, scope: ![[BAR_DBG]]{{.*}}DISPFlagDefinition{{.*}}, declaration: ![[BAR_METHOD_DECL_DBG:[0-9]+]]
// CHECK-DAG: ![[BAR_METHOD_DECL_DBG]] = !DISubprogram(name: "bar_method"{{.*}}, scope: ![[BAR_DBG]]
    func bar_method() {}
}

// CHECK: ![[FUNC_DEF_DBG]] = distinct !DISubprogram(name: "a_function"
// CHECK-NOT: declaration
// CHECK-SAME: DISPFlagDefinition
// CHECK-NOT: declaration
// CHECK-SAME: )
func a_function() {}

