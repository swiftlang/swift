// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// Key paths rooted at existential metatypes.

protocol Scope {
  static var keys: [Int] { get }
  static var counter: Int { get set }
  static subscript(i: Int) -> Int { get }
}
extension Scope {
  static var tag: String { "\(Self.self)" }
}

struct A: Scope {
  static var keys: [Int] { [1, 2, 3] }
  static var counter = 0
  static subscript(i: Int) -> Int { i * 10 }
}
struct B: Scope {
  static var keys: [Int] { [4] }
  static var counter = 100
  static subscript(i: Int) -> Int { i * 20 }
}

let keys: KeyPath<any Scope.Type, [Int]> = \.keys
let count: KeyPath<any Scope.Type, Int> = \.keys.count
let tag: KeyPath<any Scope.Type, String> = \.tag
let sub: KeyPath<any Scope.Type, Int> = \.[2]
let counter: ReferenceWritableKeyPath<any Scope.Type, Int> = \.counter
let identity: KeyPath<any Scope.Type, any Scope.Type> = \.self

for m: any Scope.Type in [A.self, B.self] {
  print(m[keyPath: keys], m[keyPath: count], m[keyPath: tag], m[keyPath: sub],
        m[keyPath: identity])
}
// CHECK: [1, 2, 3] 3 A 20 A
// CHECK: [4] 1 B 40 B

let scopeA: any Scope.Type = A.self
scopeA[keyPath: counter] += 5
// CHECK: 5 100
print(A.counter, B.counter)

let keysAgain: KeyPath<any Scope.Type, [Int]> = \.keys
// CHECK: true true
print(keys == keysAgain, keys.hashValue == keysAgain.hashValue)

let keysFn: (any Scope.Type) -> [Int] = \.keys
// CHECK: [4]
print(keysFn(B.self))

// MARK: - Class-constrained existential metatype root

class C {
  static let staticN = 42
  static var staticMutable = 0
  class var overridable: Int { 1 }
}
protocol P: C {
  static var k: Int { get }
}
final class D: C, P {
  override class var overridable: Int { 2 }
  static var k: Int { 7 }
}

let staticN: KeyPath<any P.Type, Int> = \.staticN
let staticMutable: ReferenceWritableKeyPath<any P.Type, Int> = \.staticMutable
let overridable: KeyPath<any P.Type, Int> = \.overridable
let k: KeyPath<any P.Type, Int> = \.k

let dType: any P.Type = D.self
dType[keyPath: staticMutable] = 11
// CHECK: 42 11 2 7
print(dType[keyPath: staticN], dType[keyPath: staticMutable],
      dType[keyPath: overridable], dType[keyPath: k])

protocol Marker {}
extension D: Marker {}
let composed: KeyPath<any (C & Marker).Type, Int> = \.overridable
// CHECK: 2
print((D.self as any (C & Marker).Type)[keyPath: composed])
