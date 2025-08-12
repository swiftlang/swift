// RUN: %empty-directory(%t)
// RUN: %clang -c -std=c++17 -v %target-cc-options %target-threading-opt -g -O0 -isysroot %sdk %S/Inputs/isPrespecialized.cpp -o %t/isPrespecialized.o -I %swift-include-dir -I %swift_src_root/include/ -I %swift_src_root/stdlib/public/SwiftShims/ -I %llvm_src_root/include -I %llvm_obj_root/include -L %swift-include-dir/../lib/swift/macosx

// RUN: %target-build-swift %S/Inputs/struct-public-nonfrozen-1argument.swift -emit-module -emit-library -module-name Module -Xfrontend -prespecialize-generic-metadata -target %module-target-future -emit-module-path %t/Module.swiftmodule -o %t/%target-library-name(Module)

// RUN: %target-build-swift -v %mcp_opt %s %S/Inputs/main.swift %S/Inputs/consume-logging-metadata-value.swift %t/isPrespecialized.o -import-objc-header %S/Inputs/isPrespecialized.h -Xfrontend -prespecialize-generic-metadata -target %module-target-future -lc++ -I %t -L %t -lModule -L %swift-include-dir/../lib/swift/macosx -sdk %sdk -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s


// REQUIRES: OS=macosx
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: swift_test_mode_optimize
// UNSUPPORTED: swift_test_mode_optimize_size
// UNSUPPORTED: remote_run

import Module

func ptr<T>(to ty: T.Type) -> UnsafeMutableRawPointer {
    UnsafeMutableRawPointer(mutating: unsafePointerToMetadata(of: ty))!
}

struct FirstUsageDynamic {}
struct FirstUsageStatic {}

@inline(never)
func consumeType<T>(oneArgument: Module.OneArgument<T>.Type, line: UInt = #line) {
  consumeType(oneArgument, line: line)
}

@inline(never)
func consumeType_OneArgumentAtFirstUsageStatic_Static(line: UInt = #line) {
  consumeType(OneArgument<FirstUsageStatic>.self, line: line)
}

@inline(never)
func consumeType_OneArgumentAtFirstUsageStatic_Dynamic(line: UInt = #line) {
  consumeType(oneArgument: OneArgument<FirstUsageStatic>.self, line: line)
}

@inline(never)
func consumeType_OneArgumentAtFirstUsageDynamic_Static(line: UInt = #line) {
  consumeType(OneArgument<FirstUsageDynamic>.self, line: line)
}

@inline(never)
func consumeType_OneArgumentAtFirstUsageDynamic_Dynamic(line: UInt = #line) {
  consumeType(oneArgument: OneArgument<FirstUsageDynamic>.self, line: line)
}

@inline(never)
func doit() {
  // CHECK: [[STATIC_METADATA_ADDRESS:[0-9a-f]+]] @ [[@LINE+1]]
  consumeType_OneArgumentAtFirstUsageStatic_Static()
  // CHECK: [[STATIC_METADATA_ADDRESS]] @ [[@LINE+1]]
  consumeType_OneArgumentAtFirstUsageStatic_Dynamic()
  // CHECK: [[STATIC_METADATA_ADDRESS]] @ [[@LINE+1]]
  consumeType_OneArgumentAtFirstUsageStatic_Dynamic()
  // CHECK: [[DYNAMIC_METADATA_ADDRESS:[0-9a-f]+]] @ [[@LINE+1]]
  consumeType_OneArgumentAtFirstUsageDynamic_Dynamic()
  // CHECK: [[DYNAMIC_METADATA_ADDRESS:[0-9a-f]+]] @ [[@LINE+1]]
  consumeType_OneArgumentAtFirstUsageDynamic_Dynamic()
  // CHECK: [[DYNAMIC_METADATA_ADDRESS]] @ [[@LINE+1]]
  consumeType_OneArgumentAtFirstUsageDynamic_Static()

  let staticMetadata = ptr(to: OneArgument<FirstUsageStatic>.self)
  // CHECK: [[STATIC_METADATA_ADDRESS]] @ [[@LINE+1]]
  print(staticMetadata, "@", #line)
  assert(isStaticallySpecializedGenericMetadata(staticMetadata))
  assert(!isCanonicalStaticallySpecializedGenericMetadata(staticMetadata))

  let dynamicMetadata = ptr(to: OneArgument<FirstUsageDynamic>.self)
  // CHECK: [[DYNAMIC_METADATA_ADDRESS]] @ [[@LINE+1]]
  print(dynamicMetadata, "@", #line)
  assert(!isStaticallySpecializedGenericMetadata(dynamicMetadata))
  assert(!isCanonicalStaticallySpecializedGenericMetadata(dynamicMetadata))
}
