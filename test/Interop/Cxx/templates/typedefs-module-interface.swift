// RUN: %target-swift-ide-test -print-module -module-to-print=Typedefs -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: struct Avocado {
// CHECK:   init()
// CHECK:   mutating func taste() -> Int32
// CHECK: }
// CHECK: struct Banana {
// CHECK:   init()
// CHECK:   mutating func taste() -> Int32
// CHECK: }
// CHECK: struct Cucumber {
// CHECK:   init()
// CHECK:   mutating func taste() -> Int32
// CHECK: }
// CHECK: struct DragonFruit {
// CHECK:   init()
// CHECK:   mutating func taste() -> Int32
// CHECK: }
// CHECK: struct __Peel__Avocado__ {
// CHECK:   var fruit: Avocado
// CHECK:   init()
// CHECK:   init(fruit: Avocado)
// CHECK:   mutating func peeledTaste() -> Int32
// CHECK: }
// CHECK: struct __Peel__Banana__ {
// CHECK:   var fruit: Banana
// CHECK:   init()
// CHECK:   init(fruit: Banana)
// CHECK:   mutating func peeledTaste() -> Int32
// CHECK: }
// CHECK: struct __Peel__DragonFruit__ {
// CHECK:   var fruit: DragonFruit
// CHECK:   init()
// CHECK:   init(fruit: DragonFruit)
// CHECK:   mutating func peeledTaste() -> Int32
// CHECK: }
// CHECK: struct __Peel__Cucumber__ {
// CHECK:   var fruit: Cucumber
// CHECK:   init()
// CHECK:   init(fruit: Cucumber)
// CHECK:   mutating func peeledTaste() -> Int32
// CHECK: }
// CHECK: typealias PeeledAvocado = __Peel__Avocado__
// CHECK: typealias PeeledBanana = __Peel__Banana__
// CHECK: typealias OtherPeeledBanana = __Peel__Banana__
// CHECK: typealias PeeledCucumber = __Peel__Cucumber__
// CHECK: typealias PeeledDragonFruit = __Peel__DragonFruit__

// CHECK-NOT: NotImportedIncompleteType
// CHECK-NOT: NotImported
// CHECK-NOT: AddInt


