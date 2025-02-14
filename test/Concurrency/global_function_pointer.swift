// RUN: %target-swift-frontend -enable-experimental-feature SymbolLinkageMarkers -Xllvm -sil-print-types -emit-sil -target %target-swift-5.1-abi-triple -parse-as-library %s | %FileCheck %s --check-prefix=CHECK-SIL
// RUN: %target-build-swift -enable-experimental-feature SymbolLinkageMarkers -target %target-swift-5.1-abi-triple -Xfrontend -parse-as-library %s -o %t_binary
// RUN: %target-codesign %t_binary
// RUN: %target-run %t_binary | %FileCheck %s --check-prefix=CHECK-EXEC

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=ios
// REQUIRES: swift_in_compiler

// rdar://76038845
// REQUIRES: concurrency_runtime
// REQUIRES: swift_feature_SymbolLinkageMarkers
// UNSUPPORTED: back_deployment_runtime

@MainActor
var foo: Int = 42

func asyncFunc() async {
  print("Hello World!")
}

public func callAsync() async {
  await asyncFunc()
}

// CHECK-SIL-LABEL: sil_global @$s23global_function_pointer5gfptryyYacvp : $@async @callee_guaranteed () -> () = {
// CHECK-SIL:         %0 = function_ref @$s23global_function_pointer9callAsyncyyYaF : $@convention(thin) @async () -> ()
// CHECK-SIL-NEXT:    %initval = thin_to_thick_function %0 : $@convention(thin) @async () -> () to $@async @callee_guaranteed () -> ()
// CHECK-SIL-NEXT:  }

@_section("__DATA,__mysection")
public var gfptr = callAsync

@main struct MyProgram {
  static func main() async throws {
    print("\(foo)")
    foo += 1
    await gfptr()
    print("\(foo)")
  }
}

// CHECK-EXEC: 42
// CHECK-EXEC-NEXT: Hello World!
// CHECK-EXEC-NEXT: 43

