// RUN: %target-swift-frontend -emit-ir -g %s -o - | %FileCheck %s
//
// A top-level closure is expected to have the main module as scope and not the
// top_level_code function.
//
// CHECK: define {{.*}}@"$s4mainSiycfU_"() {{.*}} !dbg ![[CLOSURE:[0-9]+]] {
// CHECK: ![[MOD:[0-9]+]] = !DIModule(scope: null, name: "main"
// CHECK: ![[CLOSURE]] = distinct !DISubprogram({{.*}}scope: ![[MOD]],
let closure = { 42 }
closure()
