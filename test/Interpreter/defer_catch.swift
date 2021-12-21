// RUN: %target-run-simple-swift | %FileCheck %s
var x = 17
var y = 38

enum BadTimes: Error { case badTimes }

func bar(z: Int) throws -> Int {
    if z & 1 != 0 { throw BadTimes.badTimes }
    return 6
}

func bas(z: Int) throws -> Int {
    if z & 2 != 0 { throw BadTimes.badTimes }
    return 79
}

func zim(z: Int) throws {
    if z & 4 != 0 { throw BadTimes.badTimes }
}

func foo(z: Int) throws {
    x = try bar(z: z)
    defer catch { x = 17 }

    y = try bas(z: z)
    defer catch { y = 38 }

    try zim(z: z)

    print("done")
}

// CHECK: x: 17 y: 38
print("x: \(x) y: \(y)")
_ = try? foo(z: 4)
// CHECK-NEXT: x: 17 y: 38
print("x: \(x) y: \(y)")
_ = try? foo(z: 2)
// CHECK-NEXT: x: 17 y: 38
print("x: \(x) y: \(y)")
_ = try? foo(z: 1)
// CHECK-NEXT: x: 17 y: 38
print("x: \(x) y: \(y)")
_ = try? foo(z: 0)
// CHECK-NEXT: done
// CHECK-NEXT: x: 6 y: 79
print("x: \(x) y: \(y)")
