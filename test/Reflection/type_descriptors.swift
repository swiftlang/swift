// UNSUPPORTED: OS=windows-msvc
//
// LC_DYLD_CHAINED_FIXUPS decode not currently supported (default on visionOS)
// UNSUPPORTED: OS=xros
//
// Temporarily disable on AArch64 Linux (rdar://88451721)
// UNSUPPORTED: OS=linux-gnu && CPU=aarch64

// rdar://100558042
// UNSUPPORTED: CPU=arm64e

// RUN: %empty-directory(%t)

// RUN: %target-build-swift -Xfrontend -prespecialize-generic-metadata %s -o %t/type_descriptors
// RUN: %target-swift-reflection-dump %t/type_descriptors | %t/type_descriptors | %FileCheck %s

// swift-reflection-dump dumps types in the order they appear in the binary. We
// don't want to encode that order in the tests and break the test just because
// the order changed.
//
// To address that, we pass the output of swift-reflection-dump through this
// program, which serves the dual purpose of containing a bunch of types we want
// to dump as part of the test, and also getting the dump output into an
// order-independent form. This bit of code reads swift-reflection-dump's output
// and re-outputs the type dump section in a consistently sorted order.
var sawTypes = false
var outputChunks: [String] = []
while let line = readLine(strippingNewline: false) {
  if !sawTypes {
    if line.starts(with: "TYPES:") {
      sawTypes = true
      _ = readLine() // Skip over the ==== divider.
    }
    continue
  }

  // Skip blank lines.
  if line == "\n" {
    continue
  }

  if line.starts(with: "  ") && !outputChunks.isEmpty {
    outputChunks[outputChunks.count - 1] += line
  } else {
    outputChunks.append(line)
  }
}
for chunk in outputChunks.sorted() {
  print(chunk)
}

// Helper function to get the compiler to emit a prespecialization.
@inline(never)
private func prespecialize<T>(_ t: T) {
    withExtendedLifetime(t) { t in }
}

// ===========================
// BEGIN TYPES AND CHECK LINES
// ===========================

// Types are sorted by kind and name (e.g. 'struct A' comes before 'struct B',
// but 'enum Z' comes before either). New types need to be added in the right
// place.


// Begin classes.


class GenericClass1<T> {
  var x: T?
}
// CHECK: class type_descriptors.GenericClass1:
// CHECK-NEXT:   immediate members: 6
// CHECK-NEXT:   fields: 1
// CHECK-NEXT:   has vtable: true
// CHECK-NEXT:   generic parameters: 1
// CHECK-NEXT:   generic requirements: 0
// CHECK-NEXT:   key arguments: 1

class GenericClass2<T, U> {
  var x: T?
  var y: U?
}
// CHECK: class type_descriptors.GenericClass2:
// CHECK-NEXT:   immediate members: 11
// CHECK-NEXT:   fields: 2
// CHECK-NEXT:   has vtable: true
// CHECK-NEXT:   generic parameters: 2
// CHECK-NEXT:   generic requirements: 0
// CHECK-NEXT:   key arguments: 2

class GenericClassWithPrespecializations<T> {
  var x: T?
}
prespecialize(GenericClassWithPrespecializations<Int>.self)
// CHECK: class type_descriptors.GenericClassWithPrespecializations:
// CHECK-NEXT:   immediate members: 6
// CHECK-NEXT:   fields: 1
// CHECK-NEXT:   has vtable: true
// CHECK-NEXT:   generic parameters: 1
// CHECK-NEXT:   generic requirements: 0
// CHECK-NEXT:   key arguments: 1
// CHECK-NEXT:   canonical metadata prespecializations: 1

class GenericSubclass<T, U>: GenericClass1<T> {
  var y: U?
}
// CHECK: class type_descriptors.GenericSubclass:
// CHECK-NEXT:   superclass: type_descriptors.GenericClass1<A>
// CHECK-NEXT:   immediate members: 6
// CHECK-NEXT:   fields: 1
// CHECK-NEXT:   has vtable: true
// CHECK-NEXT:   has override table: true
// CHECK-NEXT:   generic parameters: 2
// CHECK-NEXT:   generic requirements: 0
// CHECK-NEXT:   key arguments: 2

class GenericSubclassOfNongeneric<T>: NongenericClass {
  var y: T?
}
// CHECK: class type_descriptors.GenericSubclassOfNongeneric:
// CHECK-NEXT:   superclass: type_descriptors.NongenericClass
// CHECK-NEXT:   immediate members: 5
// CHECK-NEXT:   fields: 1
// CHECK-NEXT:   has vtable: true
// CHECK-NEXT:   has override table: true
// CHECK-NEXT:   generic parameters: 1
// CHECK-NEXT:   generic requirements: 0
// CHECK-NEXT:   key arguments: 1
class NongenericClass {
  var x: Int?
}
// CHECK: class type_descriptors.NongenericClass:
// CHECK-NEXT:   immediate members: 5
// CHECK-NEXT:   fields: 1
// CHECK-NEXT:   has vtable: true

class NongenericSubclass: NongenericClass {
  var y: Int?
}
// CHECK: class type_descriptors.NongenericSubclass:
// CHECK-NEXT:   superclass: type_descriptors.NongenericClass
// CHECK-NEXT:   immediate members: 4
// CHECK-NEXT:   fields: 1
// CHECK-NEXT:   has vtable: true
// CHECK-NEXT:   has override table: true

class NongenericSubclassOfGeneric: GenericClass1<Int> {
  var y: Int?
}
// CHECK: class type_descriptors.NongenericSubclassOfGeneric:
// CHECK-NEXT:   superclass: type_descriptors.GenericClass1<Swift.Int>
// CHECK-NEXT:   immediate members: 4
// CHECK-NEXT:   fields: 1
// CHECK-NEXT:   has vtable: true
// CHECK-NEXT:   has override table: true
// CHECK-NEXT:   has singleton metadata initialization: true


// Begin enums.


enum GenericConditionallyCopyableEnum<T: ~Copyable>: ~Copyable {
  case a(T), b
}
extension GenericConditionallyCopyableEnum: Copyable where T: Copyable {}
// CHECK: enum type_descriptors.GenericConditionallyCopyableEnum:
// CHECK-NEXT:   payload cases: 1
// CHECK-NEXT:   empty cases: 1
// CHECK-NEXT:   generic parameters: 1
// CHECK-NEXT:   generic requirements: 1
// CHECK-NEXT:   key arguments: 1
// CHECK-NEXT:   has conditional inverted protocols: true
// CHECK-NEXT:   invertible protocols: ~Copyable

enum GenericEnum1<T> {
  case a(T), b
}
// CHECK: enum type_descriptors.GenericEnum1:
// CHECK-NEXT:   payload cases: 1
// CHECK-NEXT:   empty cases: 1
// CHECK-NEXT:   generic parameters: 1
// CHECK-NEXT:   generic requirements: 0
// CHECK-NEXT:   key arguments: 1

enum GenericEnum2<T, U> {
  case a(T), b(U), c
}
// CHECK: enum type_descriptors.GenericEnum2:
// CHECK-NEXT:   payload cases: 2
// CHECK-NEXT:   empty cases: 1
// CHECK-NEXT:   generic parameters: 2
// CHECK-NEXT:   generic requirements: 0
// CHECK-NEXT:   key arguments: 2

enum GenericEnumWithPrespecializations<T> {
  case a(T), b
}
prespecialize(GenericEnumWithPrespecializations<Int>.self)
prespecialize(GenericEnumWithPrespecializations<NongenericEnum0>.self)
// CHECK: enum type_descriptors.GenericEnumWithPrespecializations:
// CHECK-NEXT:   payload cases: 1
// CHECK-NEXT:   empty cases: 1
// CHECK-NEXT:   generic parameters: 1
// CHECK-NEXT:   generic requirements: 0
// CHECK-NEXT:   key arguments: 1
// CHECK-NEXT:   canonical metadata prespecializations: 2

enum GenericNoncopyableEnum<T>: ~Copyable {
  case a, b
}
// CHECK: enum type_descriptors.GenericNoncopyableEnum:
// CHECK-NEXT:   payload cases: 0
// CHECK-NEXT:   empty cases: 2
// CHECK-NEXT:   generic parameters: 1
// CHECK-NEXT:   generic requirements: 0
// CHECK-NEXT:   key arguments: 1
// CHECK-NEXT:   invertible protocols: ~Copyable

enum GenericNoncopyableEnumNoncopyableParam<T: ~Copyable>: ~Copyable {
  case a(T), b
}
// CHECK: enum type_descriptors.GenericNoncopyableEnumNoncopyableParam:
// CHECK-NEXT:   payload cases: 1
// CHECK-NEXT:   empty cases: 1
// CHECK-NEXT:   generic parameters: 1
// CHECK-NEXT:   generic requirements: 1
// CHECK-NEXT:   key arguments: 1
// CHECK-NEXT:   invertible protocols: ~Copyable

enum NongenericEnum0 {
  case a, b, c, d
}
// CHECK: enum type_descriptors.NongenericEnum0:
// CHECK-NEXT:   payload cases: 0
// CHECK-NEXT:   empty cases: 4

enum NongenericEnum1 {
  case a, b, c(Int)
}
// CHECK: enum type_descriptors.NongenericEnum1:
// CHECK-NEXT:   payload cases: 1
// CHECK-NEXT:   empty cases: 2

enum NongenericEnum2 {
  case a, b, c(Int), d(Int)
}
// CHECK: enum type_descriptors.NongenericEnum2:
// CHECK-NEXT:   payload cases: 2
// CHECK-NEXT:   empty cases: 2

enum NongenericNoncopyableEnum: ~Copyable {
  case a(Int), b, c
}
// CHECK: enum type_descriptors.NongenericNoncopyableEnum:
// CHECK-NEXT:   payload cases: 1
// CHECK-NEXT:   empty cases: 2
// CHECK-NEXT:   invertible protocols: ~Copyable


// Begin structs.


struct GenericConditionallyCopyableStruct<T: ~Copyable>: ~Copyable {
  var a: T
  var b: T
}
extension GenericConditionallyCopyableStruct: Copyable where T: Copyable {}
// CHECK: struct type_descriptors.GenericConditionallyCopyableStruct:
// CHECK-NEXT:   fields: 2
// CHECK-NEXT:   generic parameters: 1
// CHECK-NEXT:   generic requirements: 1
// CHECK-NEXT:   key arguments: 1
// CHECK-NEXT:   has conditional inverted protocols: true
// CHECK-NEXT:   invertible protocols: ~Copyable

struct GenericNoncopyableStruct<T>: ~Copyable {
  var a: Int
  var b: Int
}
// CHECK: struct type_descriptors.GenericNoncopyableStruct:
// CHECK-NEXT:   fields: 2
// CHECK-NEXT:   generic parameters: 1
// CHECK-NEXT:   generic requirements: 0
// CHECK-NEXT:   key arguments: 1
// CHECK-NEXT:   invertible protocols: ~Copyable

struct GenericNoncopyableStructNoncopyableParam<T: ~Copyable>: ~Copyable {
  var a: T
  var b: T
}
// CHECK: struct type_descriptors.GenericNoncopyableStructNoncopyableParam:
// CHECK-NEXT:   fields: 2
// CHECK-NEXT:   generic parameters: 1
// CHECK-NEXT:   generic requirements: 1
// CHECK-NEXT:   key arguments: 1
// CHECK-NEXT:   invertible protocols: ~Copyable

struct GenericStruct1<T> {
  var a: T
  var b: Int
  var c: Int
}
// CHECK: struct type_descriptors.GenericStruct1:
// CHECK-NEXT:   fields: 3
// CHECK-NEXT:   generic parameters: 1
// CHECK-NEXT:   generic requirements: 0
// CHECK-NEXT:   key arguments: 1

struct GenericStruct2<T, U> {
  var a: T
  var b: U
}
// CHECK: struct type_descriptors.GenericStruct2:
// CHECK-NEXT:   fields: 2
// CHECK-NEXT:   generic parameters: 2
// CHECK-NEXT:   generic requirements: 0
// CHECK-NEXT:   key arguments: 2

struct GenericStructWithPrespecializations<T> {
  var a: T
  var b: Int
  var c: Int
}
prespecialize(GenericStructWithPrespecializations<Int>.self)
prespecialize(GenericStructWithPrespecializations<NongenericStruct>.self)
// CHECK: struct type_descriptors.GenericStructWithPrespecializations:
// CHECK-NEXT:   fields: 3
// CHECK-NEXT:   generic parameters: 1
// CHECK-NEXT:   generic requirements: 0
// CHECK-NEXT:   key arguments: 1
// CHECK-NEXT:   canonical metadata prespecializations: 2

struct NongenericNoncopyableStruct: ~Copyable {
  var a: Int
  var b: Int
}
// CHECK: struct type_descriptors.NongenericNoncopyableStruct:
// CHECK-NEXT:   fields: 2
// CHECK-NEXT:   invertible protocols: ~Copyable

struct NongenericStruct {
  var a: Int
  var b: Int
}
// CHECK: struct type_descriptors.NongenericStruct:
// CHECK-NEXT:   fields: 2
