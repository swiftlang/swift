// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -enable-experimental-feature Extern -emit-ir -module-name Extern | %FileCheck %s

// REQUIRES: swift_feature_Extern

// CHECK: declare {{.*}} void @"$s6Extern7import1yyF"() [[EA1:#[0-9]+]]
@_extern(wasm, module: "m0", name: "import1")
func import1()

// CHECK: declare {{.*}} i32 @"$s6Extern20import2WithReturnInts5Int32VyF"() [[EA2:#[0-9]+]]
@_extern(wasm, module: "m0", name: "import2")
func import2WithReturnInt() -> Int32

// CHECK: declare {{.*}} void @"$s6Extern16import3TakingIntyys5Int32VF"(i32) [[EA3:#[0-9]+]]
@_extern(wasm, module: "m0", name: "import3")
func import3TakingInt(_: Int32)

func test() {
    import1()
    _ = import2WithReturnInt()
    import3TakingInt(0)
}

test()

// CHECK: attributes [[EA1]] = {{{.*}} "wasm-import-module"="m0" "wasm-import-name"="import1" {{.*}}}
// CHECK: attributes [[EA2]] = {{{.*}} "wasm-import-module"="m0" "wasm-import-name"="import2" {{.*}}}
// CHECK: attributes [[EA3]] = {{{.*}} "wasm-import-module"="m0" "wasm-import-name"="import3" {{.*}}}
