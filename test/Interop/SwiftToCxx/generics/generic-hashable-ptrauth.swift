// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend %t/input.swift -module-name Input -clang-header-expose-decls=all-public -enable-experimental-feature GenerateBindingsForHashableRequirementsInCXX -typecheck -verify -emit-clang-header-path %t/input.h
// RUN: %target-interop-build-clangxx -std=gnu++20 -target arm64e-apple-macosx13.0 -O1 -S -emit-llvm %t/test.cpp -I %t -o - | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_GenerateBindingsForHashableRequirementsInCXX

// The conformance execution-context API requires a process-dependent signed
// protocol descriptor on arm64e. Lock in both the key and the runtime's
// ProtocolDescriptor string discriminator.
// CHECK-LABEL: define{{.*}} ptr @lookupHashableConformance(
// CHECK: call i64 @llvm.ptrauth.sign(i64 {{.*}}, i32 3, i64 59657)

//--- input.swift
public func acceptHashable<Value: Hashable>(_ value: Value) {}

//--- test.cpp
#include "input.h"

extern "C" void *lookupHashableConformance(void *typeMetadata) {
  return swift::_impl::lookupConformance(
             typeMetadata, swift::_impl::HashableProtocolDescriptor::get())
      .witnessTable;
}
