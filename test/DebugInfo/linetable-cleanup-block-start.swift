// RUN: %target-swift-frontend %s -emit-irgen -g -o - \
// RUN:    -module-name M -target %target-swift-5.1-abi-triple \
// RUN:    -parse-as-library | %FileCheck %s

// REQUIRES: concurrency

// A cleanup at the start of a block must not reuse the location of the
// previously emitted block. The error block of a `try` is emitted before the
// normal block.

class C {}
func g(_ c: C) throws -> Int { 42 }
nonisolated(nonsending) func gAsync() async throws -> Int { 42 }
func use(_ x: Int) {}

// CHECK-LABEL: define {{.*}} @"$s1M4testyyKF"(
// CHECK:         call {{.*}} @"$s1M1gySiAA1CCKF"(
// CHECK:         br i1
// CHECK:         call void @swift_release({{.*}}){{.*}}, !dbg ![[SYNC_NORMAL:[0-9]+]]
public func test() throws {
  do {
    let x = try g(C())
    use(x)
  } catch {
    throw error
  }
}

// CHECK-LABEL: define {{.*}} @"$s1M9testAsyncyyYaKF"(
// CHECK:         @llvm.coro.suspend.async{{.*}} @"$s1M6gAsyncSiyYaKF"
// CHECK:         br i1
// CHECK:         call void @swift_release({{.*}}){{.*}}, !dbg ![[ASYNC_NORMAL:[0-9]+]]
@MainActor
public func testAsync() async throws {
  do {
    let x = try await gAsync()
    use(x)
  } catch {
    throw error
  }
}

// CHECK-DAG: ![[SYNC_NORMAL]] = !DILocation(line: 0,
// CHECK-DAG: ![[ASYNC_NORMAL]] = !DILocation(line: 0,
