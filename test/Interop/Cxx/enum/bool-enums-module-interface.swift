// RUN: %target-swift-ide-test -print-module -module-to-print=BoolEnums -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// TODO: these should be enums eventually (especially the enum class).

// CHECK:       struct Maybe : Hashable, Equatable, RawRepresentable {
// CHECK-NEXT:    init(_ rawValue: Bool)
// CHECK-NEXT:    init(rawValue: Bool)
// CHECK-NEXT:    var rawValue: Bool
// CHECK-NEXT:	  typealias RawValue = Bool
// CHECK-NEXT:  }
// CHECK:       var No: Maybe { get }
// CHECK:       var Yes: Maybe { get }

// CHECK:       struct BinaryNumbers : Hashable, Equatable, RawRepresentable {
// CHECK-NEXT:    init(_ rawValue: Bool)
// CHECK-NEXT:    init(rawValue: Bool)
// CHECK-NEXT:	  var rawValue: Bool
// CHECK-NEXT:    typealias RawValue = Bool
// CHECK-NEXT:  }
// CHECK:       var One: BinaryNumbers { get }
// CHECK:       var Zero: BinaryNumbers { get }

// CHECK: enum EnumClass : Bool {
// CHECK:   init?(rawValue: Bool)
// CHECK:   var rawValue: Bool { get }
// CHECK:   typealias RawValue = Bool
// CHECK:   case Foo
// CHECK:   case Bar
// CHECK: }

// CHECK:       struct WrapperStruct {
// CHECK-NEXT:    init()
// TODO: where is "A" and "B"? They should be member variables.
// CHECK-NEXT:    struct InnerBoolEnum : Hashable, Equatable, RawRepresentable {
// CHECK-NEXT:      init(_ rawValue: Bool)
// CHECK-NEXT:      init(rawValue: Bool)
// CHECK-NEXT:      var rawValue: Bool
// CHECK-NEXT:      typealias RawValue = Bool
// CHECK-NEXT:    }
// CHECK-NEXT:  }
