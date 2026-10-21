// RUN: %target-swift-frontend %s -parse-as-library -emit-ir -module-name Expose | %FileCheck %s
// RUN: %target-swift-frontend %s -parse-as-library -emit-ir -module-name Expose | grep -c '"wasm-export-name"' | %FileCheck --check-prefix=COUNT %s

// Only the seven exposed globals are exported, not their addressors or
// accessors.
// COUNT: 7

// CHECK: @"$s6Expose9globalVarSivp" = {{.*}}global {{.*}} [[GLOBAL_VAR:#[0-9]+]]
@_expose(wasm)
public var globalVar = 1

// CHECK: @"$s6Expose9globalLets5Int32Vvp" = {{.*}}constant {{.*}} [[GLOBAL_LET:#[0-9]+]]
@_expose(wasm, "custom_global_let")
public let globalLet: Int32 = 2

// CHECK: @silgen_global = {{.*}}global {{.*}} [[SILGEN_GLOBAL:#[0-9]+]]
@_expose(wasm)
@_silgen_name("silgen_global")
public var silgenGlobal: Int32 = 3

public struct S {
  // CHECK: @"$s6Expose1SV9staticVarSivpZ" = {{.*}}global {{.*}} [[STATIC_VAR:#[0-9]+]]
  @_expose(wasm)
  public static var staticVar = 4
}

public func function() {}

// CHECK: @"$s6Expose12thinFunctionyyXfvp" = {{.*}}global ptr {{.*}}@"$s6Expose8functionyyF{{(\.ptrauth)?}}"{{.*}} [[THIN_FUNCTION:#[0-9]+]]
@_expose(wasm)
public var thinFunction: @convention(thin) () -> Void = function

// CHECK: @"$s6Expose13thickFunctionyycvp" = {{.*}}global %swift.function { ptr {{.*}}@"$s6Expose8functionyyF{{(\.ptrauth)?}}"{{.*}}, ptr null }, {{.*}} [[THICK_FUNCTION:#[0-9]+]]
@_expose(wasm)
public var thickFunction: () -> Void = function

// The context is allocated at run time, so the storage is initialized lazily.
// CHECK: @"$s6Expose22thickCapturingFunctionyycvp" = {{.*}}global %swift.function zeroinitializer, {{.*}} [[THICK_CAPTURING_FUNCTION:#[0-9]+]]
@_expose(wasm)
public var thickCapturingFunction: () -> Void = { [x = 1] in _ = x }

// CHECK: @"$s6Expose10notExposedSivp" = {{.*}}global {{[^#]*$}}
public var notExposed = 5

// CHECK-DAG: attributes [[GLOBAL_VAR]] = { "wasm-export-name"="$s6Expose9globalVarSivp" }
// CHECK-DAG: attributes [[GLOBAL_LET]] = { "wasm-export-name"="custom_global_let" }
// CHECK-DAG: attributes [[SILGEN_GLOBAL]] = { "wasm-export-name"="silgen_global" }
// CHECK-DAG: attributes [[STATIC_VAR]] = { "wasm-export-name"="$s6Expose1SV9staticVarSivpZ" }
// CHECK-DAG: attributes [[THIN_FUNCTION]] = { "wasm-export-name"="$s6Expose12thinFunctionyyXfvp" }
// CHECK-DAG: attributes [[THICK_FUNCTION]] = { "wasm-export-name"="$s6Expose13thickFunctionyycvp" }
// CHECK-DAG: attributes [[THICK_CAPTURING_FUNCTION]] = { "wasm-export-name"="$s6Expose22thickCapturingFunctionyycvp" }
