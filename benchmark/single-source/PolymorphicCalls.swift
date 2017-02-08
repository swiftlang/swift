//===--- PolymorphicCalls.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/*
This benchmark is used to check the performance of polymorphic invocations.
Essentially, it checks how good a compiler can optimize virtual calls of class
methods in cases where multiple sub-classes of a given class are available.

In particular, this benchmark would benefit from a good devirtualization.
In case of applying a speculative devirtualization, it would be benefit from
applying a jump-threading in combination with the speculative devirtualization.
*/

import TestsUtils

public class A {
    let b: B
    init(b:B) {
        self.b = b
    }

    public func run1() -> Int {
        return b.f1() + b.f2() + b.f3()
    }

    public func run2() -> Int {
        return b.run()
    }
}

// B has no known subclasses
public class B {
    let x: Int
    init(x:Int) {
        self.x = x
    }
    public func f1() -> Int {
        return x + 1
    }
    public func f2() -> Int {
        return x + 11
    }
    public func f3() -> Int {
        return x + 111
    }
    public func run() -> Int {
        return f1() + f2() + f3()
    }
}


public class A1 {
    let b: B1
    public init(b:B1) {
        self.b = b
    }
    public func run1() -> Int {
        return b.f1() + b.f2() + b.f3()
    }
    public func run2() -> Int {
        return b.run()
    }
}

// B1 has 1 known subclass
public class B1 {
    func f1() -> Int { return 0 }
    func f2() -> Int { return 0 }
    func f3() -> Int { return 0 }
    public func run() -> Int {
        return f1() + f2() + f3()
    }
}

public class C1: B1 {
    let x: Int
    init(x:Int) {
        self.x = x
    }
    override public func f1() -> Int {
        return x + 2
    }
    override public func f2() -> Int {
        return x + 22
    }
    override public func f3() -> Int {
        return x + 222
    }
}


public class A2 {
    let b:B2
    public init(b:B2) {
        self.b = b
    }
    public func run1() -> Int {
        return b.f1() + b.f2() + b.f3()
    }
    public func run2() -> Int {
        return b.run()
    }
}

// B2 has 2 known subclasses
public class B2 {
    func f1() -> Int { return 0 }
    func f2() -> Int { return 0 }
    func f3() -> Int { return 0 }
    public func run() -> Int {
        return f1() + f2() + f3()
    }
}

public class C2 : B2 {
    let x: Int
    init(x:Int) {
        self.x = x
    }
    override public func f1() -> Int {
        return x + 3
    }
    override public func f2() -> Int {
        return x + 33
    }
    override public func f3() -> Int {
        return x + 333
    }
}

public class D2 : B2 {
    let x: Int
    init(x:Int) {
        self.x = x
    }
    override public func f1() -> Int {
        return x + 4
    }
    override public func f2() -> Int {
        return x + 44
    }
    override public func f3() -> Int {
        return x + 444
    }
}


public class A3 {
    let b: B3

    public init(b:B3) {
        self.b = b
    }

    public func run1() -> Int {
        return b.f1() + b.f2() + b.f3()
    }
    public func run2() -> Int {
        return b.run()
    }
}


// B3 has 3 known subclasses
public class B3 {
    func f1() -> Int { return 0 }
    func f2() -> Int { return 0 }
    func f3() -> Int { return 0 }
    public func run() -> Int {
        return f1() + f2() + f3()
    }
}


public class C3: B3 {
    let x: Int
    init(x:Int) {
        self.x = x
    }
    override public func f1() -> Int {
        return x + 5
    }
    override public func f2() -> Int {
        return x + 55
    }
    override public func f3() -> Int {
        return x + 555
    }
}
public class D3: B3 {
    let x: Int
    init(x:Int) {
        self.x = x
    }
    override public func f1() -> Int {
        return x + 6
    }
    override public func f2() -> Int {
        return x + 66
    }
    override public func f3() -> Int {
        return x + 666
    }
}
public class E3:B3 {
    let x: Int
    init(x:Int) {
        self.x = x
    }
    override public func f1() -> Int {
        return x + 7
    }
    override public func f2() -> Int {
        return x + 77
    }
    override public func f3() -> Int {
        return x + 777
    }
}
public class F3 : B3 {
    let x: Int
    init(x:Int) {
        self.x = x
    }
    override public func f1() -> Int {
        return x + 8
    }
    override public func f2() -> Int {
        return x + 88
    }
    override public func f3() -> Int {
        return x + 888
    }}

// Test the cost of polymorphic method invocation
// on a class without any subclasses
@inline(never)
func test(_ a:A, _ UPTO: Int) -> Int64 {
    var cnt: Int64 = 0
    for _ in 0..<UPTO {
        cnt += Int64(a.run2())
    }
    return cnt
}

// Test the cost of polymorphic method invocation
// on a class with 1 subclass
@inline(never)
func test(_ a:A1, _ UPTO: Int) -> Int64 {
    var cnt: Int64 = 0
    for _ in 0..<UPTO {
        cnt += Int64(a.run2())
    }
    return cnt
}

// Test the cost of polymorphic method invocation
// on a class with 2 subclasses
@inline(never)
func test(_ a:A2, _ UPTO: Int) -> Int64 {
    var cnt: Int64 = 0
    for _ in 0..<UPTO {
        cnt += Int64(a.run2())
    }
    return cnt
}

// Test the cost of polymorphic method invocation
// on a class with 2 subclasses on objects
// of different subclasses
@inline(never)
func test(_ a2_c2:A2, _ a2_d2:A2,  _ UPTO: Int) -> Int64 {
    var cnt: Int64 = 0
    for _ in 0..<UPTO/2 {
        cnt += Int64(a2_c2.run2())
        cnt += Int64(a2_d2.run2())
    }
    return cnt
}

// Test the cost of polymorphic method invocation
// on a class with 4 subclasses on objects
// of different subclasses
@inline(never)
func test(_ a3_c3: A3, _ a3_d3: A3, _ a3_e3: A3, _ a3_f3: A3, _ UPTO: Int) -> Int64 {
    var cnt: Int64  = 0
    for _ in 0..<UPTO/4 {
        cnt += Int64(a3_c3.run2())
        cnt += Int64(a3_d3.run2())
        cnt += Int64(a3_e3.run2())
        cnt += Int64(a3_f3.run2())
    }
    return cnt
}



@inline(never)
public func run_PolymorphicCalls(_ N:Int) {
    let UPTO = 10000 * N

    let a = A(b:B(x:1))
    _ = test(a, UPTO)

    let a1 = A1(b:C1(x:1))

    _ = test(a1, UPTO)

    let a2 = A2(b:C2(x:1))

    _ = test(a2, UPTO)

    let a2_c2 = A2(b:C2(x:1))
    let a2_d2 = A2(b:D2(x:1))

    _ = test(a2_c2, a2_d2, UPTO)

    let a3_c3 = A3(b:C3(x:1))
    let a3_d3 = A3(b:D3(x:1))
    let a3_e3 = A3(b:E3(x:1))
    let a3_f3 = A3(b:F3(x:1))

    _ = test(a3_c3, a3_d3, a3_e3, a3_f3, UPTO)
}
