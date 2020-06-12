// RUN: %target-swift-ide-test -print-module -module-to-print=Typedefs -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: struct _ZTS4PeelI11DragonFruitE {
// CHECK:   var fruit: DragonFruit
// CHECK:   init()
// CHECK:   init(fruit: DragonFruit)
// CHECK:   mutating func peeledTaste() -> Int32
// CHECK: }
// CHECK: struct _ZTS4PeelI6BananaE {
// CHECK:   var fruit: Banana
// CHECK:   init()
// CHECK:   init(fruit: Banana)
// CHECK:   mutating func peeledTaste() -> Int32
// CHECK: }
// CHECK: struct _ZTS4PeelI7AvocadoE {
// CHECK:   var fruit: Avocado
// CHECK:   init()
// CHECK:   init(fruit: Avocado)
// CHECK:   mutating func peeledTaste() -> Int32
// CHECK: }
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
// CHECK: func forceInstantiatingPeeledAvocado() -> _ZTS4PeelI7AvocadoE
// CHECK: typealias PeeledAvocado = _ZTS4PeelI7AvocadoE
// CHECK: typealias PeeledBanana = _ZTS4PeelI6BananaE
// CHECK: typealias OtherPeeledBanana = _ZTS4PeelI6BananaE
// CHECK: struct _ZTS4PeelI8CucumberE {
// CHECK:   var fruit: Cucumber
// CHECK:   init()
// CHECK:   init(fruit: Cucumber)
// CHECK:   mutating func peeledTaste() -> Int32
// CHECK: }
// CHECK: typealias PeeledCucumber = _ZTS4PeelI8CucumberE
// CHECK: typealias PeeledDragonFruit = _ZTS4PeelI11DragonFruitE

// CHECK-NOT: IgnoredPartialTemplateInstantiation
// CHECK-NOT: NotImportedIncompleteType
// CHECK-NOT: NotImported


