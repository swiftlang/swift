// RUN: %target-swift-ide-test -print-module -module-to-print=NestedEnums -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default | %FileCheck %s

// CHECK:      struct HasEnums {
// CHECK-NEXT:   init()
// CHECK-NEXT:   enum Scoped : [[SCOPED_ENUM_RAW_T:Int32|UInt32]] {
// CHECK-NEXT:     init?(rawValue: [[SCOPED_ENUM_RAW_T]])
// CHECK-NEXT:     var rawValue: [[SCOPED_ENUM_RAW_T]] { get }
// CHECK-NEXT:     typealias RawValue = [[SCOPED_ENUM_RAW_T]]
// CHECK-NEXT:     case S1
// CHECK-NEXT:     case S2
// CHECK-NEXT:     case S3
// CHECK-NEXT:   }
// CHECK-NEXT:   struct Unscoped : Hashable, Equatable, RawRepresentable {
// CHECK-NEXT:     init(_ rawValue: [[UNSCOPED_ENUM_RAW_T:Int32|UInt32]])
// CHECK-NEXT:     init(rawValue: [[UNSCOPED_ENUM_RAW_T]])
// CHECK-NEXT:     var rawValue: [[UNSCOPED_ENUM_RAW_T]]
// CHECK-NEXT:     typealias RawValue = [[UNSCOPED_ENUM_RAW_T]]
// CHECK-NEXT:   }
// CHECK-NEXT:   static var U1: HasEnums.Unscoped { get }
// CHECK-NEXT:   static var U2: HasEnums.Unscoped { get }
// CHECK-NEXT:   static var U3: HasEnums.Unscoped { get }
// CHECK-NEXT:   static var A1: [[ANON_ENUM_T:Int]] { get }
// CHECK-NEXT:   static var A2: [[ANON_ENUM_T]] { get }
// CHECK-NEXT:   static var A3: [[ANON_ENUM_T]] { get }
// CHECK-NEXT: }
