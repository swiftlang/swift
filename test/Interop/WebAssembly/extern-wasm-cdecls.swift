// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -emit-ir -enable-experimental-feature Extern -module-name Extern | %FileCheck %s

// REQUIRES: swift_feature_Extern

// CHECK: declare void @import1() [[EA1:#[0-9]+]]
@_extern(c)
@_extern(wasm, module: "m0", name: "import1")
func import1()

// CHECK: declare i32 @import2WithReturnInt() [[EA2:#[0-9]+]]
@_extern(c)
@_extern(wasm, module: "m0", name: "import2")
func import2WithReturnInt() -> Int32

// CHECK: declare void @import3TakingInt(i32) [[EA3:#[0-9]+]]
@_extern(c)
@_extern(wasm, module: "m0", name: "import3")
func import3TakingInt(_: Int32)

// CHECK: declare void @c_import4() [[EA4:#[0-9]+]]
@_extern(c, "c_import4")
@_extern(wasm, module: "m0", name: "import4")
func import4()

func test() {
    import1()
    _ = import2WithReturnInt()
    import3TakingInt(0)
    import4()
}

test()

// CHECK: attributes [[EA1]] = {{{.*}} "wasm-import-module"="m0" "wasm-import-name"="import1" {{.*}}}
// CHECK: attributes [[EA2]] = {{{.*}} "wasm-import-module"="m0" "wasm-import-name"="import2" {{.*}}}
// CHECK: attributes [[EA3]] = {{{.*}} "wasm-import-module"="m0" "wasm-import-name"="import3" {{.*}}}
// CHECK: attributes [[EA4]] = {{{.*}} "wasm-import-module"="m0" "wasm-import-name"="import4" {{.*}}}
