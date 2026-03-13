// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -emit-ir -module-name Expose | %FileCheck %s

import Swift

// CHECK: define{{.*}} swiftcc void @"$s6Expose8exposed1yyF"() [[EA1:#[0-9]+]]
@_expose(wasm)
public func exposed1() {
}

// CHECK: define{{.*}} swiftcc void @"$s6Expose18exposed2NotExposedyyF"() [[DEFAULT_A:#[0-9]+]]
public func exposed2NotExposed() {
}

// CHECK: define{{.*}} swiftcc void @"$s6Expose22exposed3WithCustomNameyyF"() [[EA3:#[0-9]+]]
@_expose(wasm, "exposed3_with_custom_name")
public func exposed3WithCustomName() {
}


// CHECK: define{{.*}} swiftcc void @"$s6Expose17exposed4NonCIdentyyF"() [[EA4:#[0-9]+]]
@_expose(wasm, "exposed4-non-c-ident")
public func exposed4NonCIdent() {
}

// CHECK: define{{.*}} swiftcc i32 @"$s6Expose17exposed5ReturnInts5Int32VyF"() [[EA5:#[0-9]+]]
@_expose(wasm)
public func exposed5ReturnInt() -> Int32 {
  return 0
}

// If @_cdecl is applied at the same time as @_expose, expose C thunk instead of Swift function.
// CHECK: define{{.*}} void @exposed6WithCDecl() [[EA6:#[0-9]+]]
// CHECK: define{{.*}} swiftcc void @"$s6Expose17exposed6WithCDeclyyF"() [[DEFAULT:#[0-9]+]]
@_expose(wasm)
@_cdecl("exposed6WithCDecl")
public func exposed6WithCDecl() {
}

// CHECK: define{{.*}} void @exposed7WithCDecl() [[EA7:#[0-9]+]]
// CHECK: define{{.*}} swiftcc void @"$s6Expose018exposed7WithCDecl_C13DifferentNameyyF"() [[DEFAULT_A:#[0-9]+]]
@_expose(wasm, "exposed7WithCDecl:WithDifferentName")
@_cdecl("exposed7WithCDecl")
public func exposed7WithCDecl_WithDifferentName() {
}

// CHECK-NOT: attributes [[DEFAULT_A]] = {{.*}} "wasm-export-name"="{{.*}}"
// CHECK: attributes [[EA1]] = {{{.*}} "wasm-export-name"="$s6Expose8exposed1yyF" {{.*}}}
// CHECK-NOT: attributes {{.*}} = {{{.*}} "wasm-export-name"="$s6Expose18exposed2NotExposedyyF" {{.*}}}
// CHECK: attributes [[EA3]] = {{{.*}} "wasm-export-name"="exposed3_with_custom_name" {{.*}}}
// CHECK: attributes [[EA4]] = {{{.*}} "wasm-export-name"="exposed4-non-c-ident" {{.*}}}
// CHECK: attributes [[EA5]] = {{{.*}} "wasm-export-name"="$s6Expose17exposed5ReturnInts5Int32VyF" {{.*}}}
// CHECK: attributes [[EA6]] = {{{.*}} "wasm-export-name"="exposed6WithCDecl" {{.*}}}
// CHECK: attributes [[EA7]] = {{{.*}} "wasm-export-name"="exposed7WithCDecl:WithDifferentName" {{.*}}}
