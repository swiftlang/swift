// RUN: %target-swift-frontend -emit-silgen -enable-experimental-concurrency -module-name Async -Xllvm -sil-full-demangle -parse-as-library %s | %FileCheck %s

import Swift

// CHECK-LABEL: sil hidden [async] [ossa] @$s5Async04testA0yyYF : $@convention(thin) () -> () {
func testAsync() async {}
// CHECK-LABEL: sil hidden [async] [ossa] @$s5Async11testAsyncp1SiyYF : $@convention(thin) () -> Int {
func testAsyncp1() async -> Int { return 0 }

// CHECK-LABEL: sil hidden [async] [ossa] @$s5Async5test2yyYKF : $@convention(thin) () -> @error Error {
func test2() async throws {}
// CHECK-LABEL: sil hidden [async] [ossa] @$s5Async7test2p1SiyYKF : $@convention(thin) () -> (Int, @error Error) {
func test2p1() async throws -> Int { return 0 }

// CHECK-LABEL: sil hidden [async] [ossa] @$s5Async5test3yyyyKXEYKF : $@convention(thin) (@noescape @callee_guaranteed () -> @error Error) -> @error Error {
func test3(_ cl: () throws -> ()) async rethrows {}
// CHECK-LABEL: sil hidden [async] [ossa] @$s5Async7test3p1yS2iyKXEYKF : $@convention(thin) (@noescape @callee_guaranteed () -> (Int, @error Error)) -> (Int, @error Error) {
func test3p1(_ cl: () throws -> Int) async rethrows -> Int { return try cl() }


