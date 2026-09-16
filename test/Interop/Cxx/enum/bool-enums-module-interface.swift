// RUN: %target-swift-ide-test -print-module -module-to-print=BoolEnums -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// TODO: these should be enums eventually (especially the enum class).

// CHECK:       struct Maybe : Hashable, Equatable, RawRepresentable {
// CHECK-NEXT:    init(_ rawValue: CBool)
// CHECK-NEXT:    init(rawValue: CBool)
// CHECK-NEXT:    var rawValue: CBool
// CHECK-NEXT:	  typealias RawValue = CBool
// CHECK-NEXT:  }
// CHECK:       var No: Maybe { get }
// CHECK:       var Yes: Maybe { get }

// CHECK:       struct BinaryNumbers : Hashable, Equatable, RawRepresentable {
// CHECK-NEXT:    init(_ rawValue: CBool)
// CHECK-NEXT:    init(rawValue: CBool)
// CHECK-NEXT:	  var rawValue: CBool
// CHECK-NEXT:    typealias RawValue = CBool
// CHECK-NEXT:  }
// CHECK:       var One: BinaryNumbers { get }
// CHECK:       var Zero: BinaryNumbers { get }

// CHECK: enum EnumClass : CBool {
// CHECK:   init?(rawValue: CBool)
// CHECK:   var rawValue: CBool { get }
// CHECK:   typealias RawValue = CBool
// CHECK:   case Foo
// CHECK:   case Bar
// CHECK: }

// CHECK:       struct WrapperStruct {
// CHECK-NEXT:    init()
// TODO: where is "A" and "B"? They should be member variables.
// CHECK-NEXT:    struct InnerBoolEnum : Hashable, Equatable, RawRepresentable {
// CHECK-NEXT:      init(_ rawValue: CBool)
// CHECK-NEXT:      init(rawValue: CBool)
// CHECK-NEXT:      var rawValue: CBool
// CHECK-NEXT:      typealias RawValue = CBool
// CHECK-NEXT:    }
// CHECK-NEXT:  }
