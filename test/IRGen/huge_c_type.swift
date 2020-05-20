// RUN: %target-swift-frontend -import-objc-header %S/Inputs/huge_c_type.h %s -disable-llvm-optzns -emit-ir | %FileCheck %s
// Make sure that this does not crash during LLVM's ISel. It does not like huge
// llvm::IntegerTypes.
// RUN: %target-swift-frontend -import-objc-header %S/Inputs/huge_c_type.h %s -c

// CHECK-NOT:i9535616

public func doIt(a: Thing3) {
  print(a)
}
