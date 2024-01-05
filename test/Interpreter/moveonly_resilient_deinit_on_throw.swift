// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module-path %t -enable-library-evolution -module-name moveonly_resilient_type -parse-as-library %S/Inputs/moveonly_resilient_type.swift -c -o %t/moveonly_resilient_type.o
// RUN: %target-build-swift -enable-library-evolution -module-name moveonly_resilient_type -parse-as-library %S/Inputs/moveonly_resilient_type.swift -c -o %t/moveonly_resilient_type.o
// RUN: %target-build-swift -o %t/a.out -I %t %s %t/moveonly_resilient_type.o
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

// CHECK: starting

import moveonly_resilient_type

func makeItem1() throws -> Resilient {
    return Resilient(nonthrowing: ())
}

func test1a() throws {
    // CHECK-NEXT: resilient deinit 0
    _ = try makeItem1()
}
func test1b() throws {
    // CHECK-NEXT: resilient deinit 1
    let x = try makeItem1()
}

func makeItem2(throwing: Bool) throws -> Resilient {
    return try Resilient(throwing: throwing)
}

func test2aa() throws {
    // CHECK-NEXT: resilient deinit 2
    _ = try makeItem2(throwing: false)
}

func test2ab() throws {
    _ = try makeItem2(throwing: true)
}

func test2ba() throws {
    // CHECK-NEXT: resilient deinit 3
    let x = try makeItem2(throwing: false)
}

func test2bb() throws {
    let x = try makeItem2(throwing: true)
}

func makeItem3(throwing: Bool) throws -> Resilient {
    return try Resilient(throwingAfterInit: throwing)
}

func test3aa() throws {
    // CHECK-NEXT: resilient deinit 4
    _ = try makeItem3(throwing: false)
}

func test3ab() throws {
    // CHECK-NEXT: resilient deinit 5
    _ = try makeItem3(throwing: true)
}

func test3ba() throws {
    // CHECK-NEXT: resilient deinit 6
    let x = try makeItem3(throwing: false)
}

func test3bb() throws {
    // CHECK-NEXT: resilient deinit 7
    let x = try makeItem3(throwing: true)
}

func main() {
    print("starting")
    _ = try? test1a()
    _ = try? test1b()
    _ = try? test2aa()
    _ = try? test2ab()
    _ = try? test2ba()
    _ = try? test2bb()
    _ = try? test3aa()
    _ = try? test3ab()
    _ = try? test3ba()
    _ = try? test3bb()

    // CHECK-NEXT: 8 instances in total
    print("\(Resilient.instanceCount()) instances in total")
}
main()
