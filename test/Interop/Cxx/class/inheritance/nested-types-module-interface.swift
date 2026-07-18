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
// CHECK-NEXT:     init(_ rawValue: {{CUnsignedInt|CInt}})
// CHECK-NEXT:     init(rawValue: {{CUnsignedInt|CInt}})
// CHECK-NEXT:     var rawValue: {{CUnsignedInt|CInt}}
// CHECK-NEXT:     typealias RawValue = {{CUnsignedInt|CInt}}
// CHECK-NEXT:   }
// CHECK-NEXT:   static var ea: Base.Enum { get }
// CHECK-NEXT:   static var eb: Base.Enum { get }
// CHECK-NEXT:   static var ec: Base.Enum { get }
// CHECK-NEXT:   struct Struct {
// CHECK-NEXT:     init(sa: CInt, sb: CInt)
// CHECK-NEXT:     init()
// CHECK-NEXT:     var sa: CInt
// CHECK-NEXT:     var sb: CInt
// CHECK-NEXT:   }
// CHECK-NEXT:   struct Parent {
// CHECK-NEXT:     init()
// CHECK-NEXT:     struct Child {
// CHECK-NEXT:       init(pca: CInt)
// CHECK-NEXT:       init()
// CHECK-NEXT:       var pca: CInt
// CHECK-NEXT:     }
// CHECK-NEXT:   }
// CHECK-NEXT:   struct Union {
// CHECK-NEXT:     init(ua: CInt)
// CHECK-NEXT:     init(ub: Base.Struct)
// CHECK-NEXT:     init()
// CHECK-NEXT:     var ua: CInt
// CHECK-NEXT:     var ub: Base.Struct
// CHECK-NEXT:   }
// CHECK-NEXT: }

// CHECK:      struct Derived {
// CHECK-NEXT:   init()
// CHECK-NEXT:   typealias EnumClass = Base.EnumClass
// CHECK-NEXT:   typealias Enum = Base.Enum
// CHECK-NEXT:   static var ea: Base.Enum { get }
// CHECK-NEXT:   static var eb: Base.Enum { get }
// CHECK-NEXT:   static var ec: Base.Enum { get }
// CHECK-NEXT:   typealias Struct = Base.Struct
// CHECK-NEXT:   typealias Parent = Base.Parent
// CHECK-NEXT:   typealias Union = Base.Union
// CHECK-NEXT: }

// CHECK:      struct Derived1 {
// CHECK-NEXT:   init()
// CHECK-NEXT:   typealias EnumClass = Base.EnumClass
// CHECK-NEXT:   typealias Enum = Base.Enum
// CHECK-NEXT:   static var ea: Base.Enum { get }
// CHECK-NEXT:   static var eb: Base.Enum { get }
// CHECK-NEXT:   static var ec: Base.Enum { get }
// CHECK-NEXT:   typealias Struct = Base.Struct
// CHECK-NEXT:   typealias Parent = Base.Parent
// CHECK-NEXT:   typealias Union = Base.Union
// CHECK-NEXT: }

// CHECK:      struct Derived2 {
// CHECK-NEXT:   init()
// CHECK-NEXT:   typealias EnumClass = Base.EnumClass
// CHECK-NEXT:   typealias Enum = Base.Enum
// CHECK-NEXT:   static var ea: Base.Enum { get }
// CHECK-NEXT:   static var eb: Base.Enum { get }
// CHECK-NEXT:   static var ec: Base.Enum { get }
// CHECK-NEXT:   typealias Struct = Base.Struct
// CHECK-NEXT:   typealias Parent = Base.Parent
// CHECK-NEXT:   typealias Union = Base.Union
// CHECK-NEXT: }
