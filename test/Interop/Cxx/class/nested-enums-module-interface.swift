// RUN: %target-swift-ide-test -print-module -module-to-print=NestedEnums -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default | %FileCheck %s

// CHECK:      struct Struct {
// CHECK-NEXT:   init()
// CHECK-NEXT:   enum Scoped : [[SCOPED_RAW_T:Int32|UInt32]] {
// CHECK-NEXT:     init?(rawValue: [[SCOPED_RAW_T]])
// CHECK-NEXT:     var rawValue: [[SCOPED_RAW_T]] { get }
// CHECK-NEXT:     typealias RawValue = [[SCOPED_RAW_T]]
// CHECK-NEXT:     case S1
// CHECK-NEXT:     case S2
// CHECK-NEXT:     case S3
// CHECK-NEXT:   }
// CHECK-NEXT:   struct Unscoped : Hashable, Equatable, RawRepresentable {
// CHECK-NEXT:     init(_ rawValue: [[UNSCOPED_RAW_T:Int32|UInt32]])
// CHECK-NEXT:     init(rawValue: [[UNSCOPED_RAW_T]])
// CHECK-NEXT:     var rawValue: [[UNSCOPED_RAW_T]]
// CHECK-NEXT:     typealias RawValue = [[UNSCOPED_RAW_T]]
// CHECK-NEXT:   }
// CHECK-NEXT:   static var U1: Struct.Unscoped { get }
// CHECK-NEXT:   static var U2: Struct.Unscoped { get }
// CHECK-NEXT:   static var U3: Struct.Unscoped { get }
// CHECK-NEXT:   static var A1: [[ANON_RAW_T:Int|Int32|UInt32]] { get }
// CHECK-NEXT:   static var A2: [[ANON_RAW_T]] { get }
// CHECK-NEXT:   static var A3: [[ANON_RAW_T]] { get }
// CHECK-NEXT: }

// CHECK:      enum Namespace {
// CHECK-NEXT:   enum Scoped : [[NS_SCOPED_RAW_T:Int32|UInt32]] {
// CHECK-NEXT:     init?(rawValue: [[NS_SCOPED_RAW_T]])
// CHECK-NEXT:     var rawValue: [[NS_SCOPED_RAW_T]] { get }
// CHECK-NEXT:     typealias RawValue = [[NS_SCOPED_RAW_T]]
// CHECK-NEXT:     case S1
// CHECK-NEXT:     case S2
// CHECK-NEXT:     case S3
// CHECK-NEXT:   }
// CHECK-NEXT:   struct Unscoped : Hashable, Equatable, RawRepresentable {
// CHECK-NEXT:     init(_ rawValue: [[NS_UNSCOPED_RAW_T:Int32|UInt32]])
// CHECK-NEXT:     init(rawValue: [[NS_UNSCOPED_RAW_T]])
// CHECK-NEXT:     var rawValue: [[NS_UNSCOPED_RAW_T]]
// CHECK-NEXT:     typealias RawValue = [[NS_UNSCOPED_RAW_T]]
// CHECK-NEXT:   }
// CHECK-NEXT:   static var U1: Namespace.Unscoped { get }
// CHECK-NEXT:   static var U2: Namespace.Unscoped { get }
// CHECK-NEXT:   static var U3: Namespace.Unscoped { get }
// CHECK-NEXT:   static var A1: [[NS_ANON_RAW_T:Int|Int32|UInt32]] { get }
// CHECK-NEXT:   static var A2: [[NS_ANON_RAW_T]] { get }
// CHECK-NEXT:   static var A3: [[NS_ANON_RAW_T]] { get }
// CHECK-NEXT: }
