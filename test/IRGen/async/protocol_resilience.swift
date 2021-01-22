// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-experimental-concurrency -enable-library-evolution -emit-module-path=%t/resilient_protocol.swiftmodule -module-name=resilient_protocol %S/Inputs/resilient_protocol.swift
// RUN: %target-swift-frontend -I %t -emit-ir -enable-experimental-concurrency -enable-library-evolution %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-%target-cpu %s

import resilient_protocol

public protocol MyAwaitable {
  associatedtype Result
  func wait() async -> Result

  func waitThrows() async throws -> Result

  // FIXME
  // func waitGeneric<T>(_: T) async -> Result
  // func waitGenericThrows<T>(_: T) async throws -> Result
}

// CHECK-LABEL: @"$s19protocol_resilience11MyAwaitableP4wait6ResultQzyYFTjTu" = {{(dllexport )?}}{{(protected )?}}global %swift.async_func_pointer

// CHECK-LABEL: define {{(dllexport )?}}{{(protected )?}}swiftcc void @"$s19protocol_resilience11MyAwaitableP4wait6ResultQzyYFTj"(%swift.task* %0, %swift.executor* %1, %swift.context* swiftasync %2)
