// RUN: %target-run-simple-swift(-target %target-future-triple) | %FileCheck %s

// REQUIRES: executable_test

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Metadata lookup and casting for parameterized existentials whose primary
// associated type argument is noncopyable.
// https://github.com/swiftlang/swift/issues/93024

protocol ClassP<Handle>: AnyObject {
  associatedtype Handle: ~Copyable
}

protocol ValueP<Handle> {
  associatedtype Handle: ~Copyable
}

struct NC: ~Copyable { var x: Int }

final class C: ClassP { typealias Handle = NC }
final class CI: ClassP { typealias Handle = Int }
struct V: ValueP { typealias Handle = NC }

func name<T>(_: T) -> String { "\(T.self)" }
func wrap<T>(_ t: T) -> T? { t }
func meta<T: ~Copyable>(_: T.Type) -> Any.Type { (any ClassP<T>).self }

// CHECK: any ClassP<{{.*}}NC>
print((any ClassP<NC>).self)

let erased: any ClassP = C()
let erasedI: any ClassP = CI()
// CHECK-NEXT: C as ClassP<NC>: true
print("C as ClassP<NC>:", erased as? any ClassP<NC> != nil)
// CHECK-NEXT: C as ClassP<Int>: false
print("C as ClassP<Int>:", erased as? any ClassP<Int> != nil)
// CHECK-NEXT: CI as ClassP<NC>: false
print("CI as ClassP<NC>:", erasedI as? any ClassP<NC> != nil)

let c: any ClassP<NC> = C()
// CHECK-NEXT: any ClassP<{{.*}}NC>
print(name(c))
// CHECK-NEXT: wrapped: true
print("wrapped:", wrap(c) != nil)
// CHECK-NEXT: generic metatype matches: true
print("generic metatype matches:", meta(NC.self) == (any ClassP<NC>).self)

let a: Any = V()
// CHECK-NEXT: V as ValueP<NC>: true
print("V as ValueP<NC>:", a as? any ValueP<NC> != nil)
// CHECK-NEXT: V as ValueP<Int>: false
print("V as ValueP<Int>:", a as? any ValueP<Int> != nil)
// CHECK-NEXT: any ValueP<{{.*}}NC>
print(name(V() as any ValueP<NC>))
