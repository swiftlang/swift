// RUN: %target-swift-ide-test -print-module -module-to-print=NestedTypes -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default | %FileCheck %s

// CHECK:      struct Base {
// CHECK-NEXT:   init()
// CHECK-NEXT:   enum EnumClass : CChar {
// CHECK-NEXT:     init?(rawValue: CChar)
// CHECK-NEXT:     var rawValue: CChar { get }
// CHECK-NEXT:     typealias RawValue = CChar
// CHECK-NEXT:     case eca
// CHECK-NEXT:     case ecb
// CHECK-NEXT:     case ecc
// CHECK-NEXT:   }
// CHECK-NEXT:   struct Enum : Hashable, Equatable, RawRepresentable {
// CHECK-NEXT:     init(_ rawValue: {{UInt32|Int32}})
// CHECK-NEXT:     init(rawValue: {{UInt32|Int32}})
// CHECK-NEXT:     var rawValue: {{UInt32|Int32}}
// CHECK-NEXT:     typealias RawValue = {{UInt32|Int32}}
// CHECK-NEXT:   }
// CHECK-NEXT:   struct Struct {
// CHECK-NEXT:     init()
// CHECK-NEXT:     init(sa: Int32, sb: Int32)
// CHECK-NEXT:     var sa: Int32
// CHECK-NEXT:     var sb: Int32
// CHECK-NEXT:   }
// CHECK-NEXT:   struct Parent {
// CHECK-NEXT:     init()
// CHECK-NEXT:     struct Child {
// CHECK-NEXT:       init()
// CHECK-NEXT:       init(pca: Int32)
// CHECK-NEXT:       var pca: Int32
// CHECK-NEXT:     }
// CHECK-NEXT:   }
// CHECK-NEXT:   struct Union {
// CHECK-NEXT:     init()
// CHECK-NEXT:     init(ua: Int32)
// CHECK-NEXT:     init(ub: Base.Struct)
// CHECK-NEXT:     var ua: Int32
// CHECK-NEXT:     var ub: Base.Struct
// CHECK-NEXT:   }
// CHECK-NEXT: }

// CHECK:      struct Derived {
// CHECK-NEXT:   init()
// CHECK-NEXT:   typealias EnumClass = Base.EnumClass
// CHECK-NEXT:   typealias Enum = Base.Enum
// CHECK-NEXT:   typealias Struct = Base.Struct
// CHECK-NEXT:   typealias Parent = Base.Parent
// CHECK-NEXT:   typealias Union = Base.Union
// CHECK-NEXT: }

// CHECK:      struct Derived1 {
// CHECK-NEXT:   init()
// CHECK-NEXT:   typealias EnumClass = Base.EnumClass
// CHECK-NEXT:   typealias Enum = Base.Enum
// CHECK-NEXT:   typealias Struct = Base.Struct
// CHECK-NEXT:   typealias Parent = Base.Parent
// CHECK-NEXT:   typealias Union = Base.Union
// CHECK-NEXT: }

// CHECK:      struct Derived2 {
// CHECK-NEXT:   init()
// CHECK-NEXT:   typealias EnumClass = Base.EnumClass
// CHECK-NEXT:   typealias Enum = Base.Enum
// CHECK-NEXT:   typealias Struct = Base.Struct
// CHECK-NEXT:   typealias Parent = Base.Parent
// CHECK-NEXT:   typealias Union = Base.Union
// CHECK-NEXT: }
