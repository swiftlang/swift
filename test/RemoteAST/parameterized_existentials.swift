// RUN: %target-swift-remoteast-test -disable-availability-checking %s | %FileCheck %s

// REQUIRES: swift-remoteast-test

@_silgen_name("printDynamicTypeAndAddressForExistential")
func printDynamicTypeAndAddressForExistential<T>(_: T)

@_silgen_name("stopRemoteAST")
func stopRemoteAST()

protocol Paddock<Animal> {
  associatedtype Animal
}

struct Chicken {}
struct Coop: Paddock {
  typealias Animal = Chicken
}

struct Pig {}
struct Pen: Paddock {
  typealias Animal = Pig
}

struct Field<Animal>: Paddock {}

protocol SharedYard<Animal1, Animal2, Animal3, Animal4> {
  associatedtype Animal1
  associatedtype Animal2
  associatedtype Animal3
  associatedtype Animal4
}

class Lea: SharedYard {
  typealias Animal1 = Chicken
  typealias Animal2 = Pig
  typealias Animal3 = Chicken
  typealias Animal4 = Pig

  init() {}
}

let coop = Coop()
// CHECK: Coop
printDynamicTypeAndAddressForExistential(coop as any Paddock)

// CHECK-NEXT: Coop
printDynamicTypeAndAddressForExistential(coop as any Paddock<Chicken>)

// CHECK-NEXT: Coop.Type
printDynamicTypeAndAddressForExistential(Coop.self as (any Paddock<Chicken>.Type))

// CHECK-NEXT: Coop.Type.Type.Type.Type
printDynamicTypeAndAddressForExistential(Coop.Type.Type.Type.self as (any Paddock<Chicken>.Type.Type.Type.Type))

let pen = Pen()
// CHECK-NEXT: Pen
printDynamicTypeAndAddressForExistential(pen as any Paddock)

// CHECK-NEXT: Pen
printDynamicTypeAndAddressForExistential(pen as any Paddock<Pig>)

let lea = Lea()
// CHECK-NEXT: Lea
printDynamicTypeAndAddressForExistential(lea as any SharedYard)

// CHECK-NEXT: Lea
printDynamicTypeAndAddressForExistential(lea as any SharedYard<Chicken, Pig, Chicken, Pig>)


func freeRange<Animal>(_ x: Animal.Type) {
  printDynamicTypeAndAddressForExistential(Field<Animal>() as any Paddock<Animal>)
}

// CHECK-NEXT: Field<Chicken>
freeRange(Chicken.self)
// CHECK-NEXT: Field<Pig>
freeRange(Pig.self)


stopRemoteAST()
