// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module  -target %target-swift-5.1-abi-triple -g -enable-library-evolution -emit-module-path=%t/resilient_protocol.swiftmodule -module-name=resilient_protocol %S/Inputs/resilient_protocol.swift
// RUN: %target-swift-frontend -I %t -emit-ir  -target %target-swift-5.1-abi-triple -g -enable-library-evolution %s | %FileCheck -check-prefix CHECK -check-prefix CHECK-%target-cpu -check-prefix CHECK-%target-import-type %s
// REQUIRES: concurrency

import resilient_protocol

public protocol MyAwaitable {
  associatedtype Result

  func wait() async -> Int

  func wait() async -> Result

  func waitThrows() async throws -> Int

  func waitThrows() async throws -> Result

  // FIXME
  // func waitGeneric<T>(_: T) async -> Result
  // func waitGenericThrows<T>(_: T) async throws -> Result
}

// CHECK-LABEL: @"$s19protocol_resilience11MyAwaitableP4wait6ResultQzyYaFTjTu" = {{(dllexport )?}}{{(protected )?}}global %swift.async_func_pointer

// CHECK-LABEL: @"$s19protocol_resilience19ConformsToAwaitableVyxG010resilient_A00E0AAMc" = hidden constant
// CHECK-SAME: ptr @"$s19protocol_resilience19ConformsToAwaitableVyxG010resilient_A00E0AaeFP4wait6ResultQzyYaFTWTu"

// CHECK-LABEL: define {{(dllexport )?}}{{(protected )?}}swift{{(tail)?}}cc void @"$s19protocol_resilience14callsAwaitabley6ResultQzxYa010resilient_A00D0RzlF"(ptr noalias %0, ptr swiftasync %1, ptr noalias %2, ptr %T, ptr %T.Awaitable)
// CHECK-DIRECT: ptr @"$s18resilient_protocol9AwaitableP4wait6ResultQzyYaFTjTu"
// CHECK-INDIRECT: [[TMP4:%.*]] = and i64 add (i64 ptrtoint (ptr @"\01__imp_$s18resilient_protocol9AwaitableP4wait6ResultQzyYaFTjTu" to i64), i64 1), 1
// CHECK-INDIRECT: [[TMP5:%.*]] = icmp eq i64 [[TMP4]], 0
// CHECK-INDIRECT: [[TMP6:%.*]] = and i64 add (i64 ptrtoint (ptr @"\01__imp_$s18resilient_protocol9AwaitableP4wait6ResultQzyYaFTjTu" to i64), i64 1), -2
// CHECK-INDIRECT: [[TMP7:%.*]] = inttoptr i64 [[TMP6]] to ptr
// CHECK-INDIRECT: [[TMP8:%.*]] = load ptr, ptr [[TMP7]], align 8
// CHECK-INDIRECT:              select i1 [[TMP5]], ptr inttoptr (i64 add (i64 ptrtoint (ptr @"\01__imp_$s18resilient_protocol9AwaitableP4wait6ResultQzyYaFTjTu" to i64), i64 1) to ptr), ptr [[TMP8]]
// CHECK: ret void
public func callsAwaitable<T : Awaitable>(_ t: T) async -> T.Result {
  return await t.wait()
}

// CHECK-LABEL: define {{(dllexport )?}}{{(protected )?}}swift{{(tail)?}}cc void @"$s19protocol_resilience11MyAwaitableP4wait6ResultQzyYaFTj"(ptr noalias %0, ptr swiftasync %1, ptr noalias swiftself %2, ptr %3, ptr %4)

struct ConformsToAwaitable<T> : Awaitable {
  var value: T

  func wait() async -> T {
    return value
  }
}
