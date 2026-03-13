// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module-path %t -enable-library-evolution -module-name moveonly_resilient_type -parse-as-library %S/Inputs/moveonly_resilient_type.swift -c -o %t/moveonly_resilient_type.o
// RUN: %target-build-swift -enable-library-evolution -module-name moveonly_resilient_type -parse-as-library %S/Inputs/moveonly_resilient_type.swift -c -o %t/moveonly_resilient_type.o
// RUN: %target-build-swift -o %t/a.out -I %t %s %t/moveonly_resilient_type.o
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

import moveonly_resilient_type

// CHECK: start

func test1a() throws {
    // CHECK-NEXT: resilient capture in deinit 0
    _ = ResilientCapturesInDeinit(nonthrowing: ())
}
func test1b() throws {
    // CHECK-NEXT: resilient capture in deinit 1
    let x = ResilientCapturesInDeinit(nonthrowing: ())
}
func test2a() throws {
    // CHECK-NEXT: resilient capture in deinit 2
    _ = try ResilientCapturesInDeinit(throwing: false)
}
func test2b() throws {
    // CHECK-NEXT: resilient capture in deinit 3
    let x = try ResilientCapturesInDeinit(throwing: false)
}
func test3a() throws {
    _ = try ResilientCapturesInDeinit(throwing: true)
}
func test3b() throws {
    let x = try ResilientCapturesInDeinit(throwing: true)
}
func test4a() throws {
    // CHECK-NEXT: resilient capture in deinit 4
    _ = try ResilientCapturesInDeinit(throwingAfterInit: false)
}
func test4b() throws {
    // CHECK-NEXT: resilient capture in deinit 5
    let x = try ResilientCapturesInDeinit(throwingAfterInit: false)
}
func test5a() throws {
    // CHECK-NEXT: resilient capture in deinit 6
    _ = try ResilientCapturesInDeinit(throwingAfterInit: true)
}
func test5b() throws {
    // CHECK-NEXT: resilient capture in deinit 7
    let x = try ResilientCapturesInDeinit(throwingAfterInit: true)
}

func main() {
    print("start")

    _ = try? test1a()
    _ = try? test1b()
    _ = try? test2a()
    _ = try? test2b()
    _ = try? test3a()
    _ = try? test3b()
    _ = try? test4a()
    _ = try? test4b()
    _ = try? test5a()
    _ = try? test5b()

    // CHECK-NEXT: total 8
    print("total \(ResilientCapturesInDeinit.instanceCount())")
}
main()
