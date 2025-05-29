// RUN: %target-run-simple-swift(   -enable-experimental-feature Embedded -wmo -Xfrontend -disable-access-control -runtime-compatibility-version none) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature Embedded -wmo -Xfrontend -disable-access-control -runtime-compatibility-version none) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

struct MyStruct {
  var x: Int8
  var y: Int16
  var z: Int32
}

public func test1() -> Int {
  return (\MyStruct.z)._storedInlineOffset!
}

print(test1() == 4 ? "OK!" : "FAIL") // CHECK: OK!

public func test2() -> Int {
  return MemoryLayout<MyStruct>.offset(of: \MyStruct.z)!
}

print(test2() == 4 ? "OK!" : "FAIL") // CHECK: OK!

public func test3(input: UnsafePointer<MyStruct>) -> UnsafePointer<Int32> {
  return input.pointer(to: \MyStruct.z)!
}

print(UInt(bitPattern: test3(input: UnsafePointer(bitPattern: 0x1000)!)) == 0x1004 ? "OK!" : "FAIL") // CHECK: OK!

@dynamicMemberLookup
struct StructWithDynamicMemberLookup<T>: ~Copyable {
    private var value: UnsafeMutablePointer<T>
    init(from t: T) { value = UnsafeMutablePointer<T>.allocate(capacity: 1); value.pointee = t }
    deinit { value.deallocate() }
    
    subscript<U>(dynamicMember keyPath: KeyPath<T, U>) -> UnsafeMutablePointer<U> {
        @_transparent
        get {
            return UnsafeMutablePointer(mutating: self.value.pointer(to: keyPath).unsafelyUnwrapped)
        }
    }
}

public func test4(arg: borrowing StructWithDynamicMemberLookup<MyStruct>) -> Int {
  return Int(arg.z.pointee) + Int(arg.y.pointee)
}

let str = StructWithDynamicMemberLookup<MyStruct>(from: MyStruct(x: 10, y: 20, z: 30))
print(str.z.pointee == 30 ? "OK!" : "FAIL") // CHECK: OK!
