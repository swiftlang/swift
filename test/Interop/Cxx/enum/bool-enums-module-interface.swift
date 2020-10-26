// RUN: %target-swift-ide-test -print-module -module-to-print=BoolEnums -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// TODO: these should be enums eventually (especially the enum class).

// CHECK:       struct Maybe : Equatable, RawRepresentable {
// CHECK-NEXT:    init(_ rawValue: Bool)
// CHECK-NEXT:    init(rawValue: Bool)
// CHECK-NEXT:    var rawValue: Bool
// CHECK-NEXT:	  typealias RawValue = Bool
// CHECK-NEXT:  }

// CHECK:       var No: Maybe { get }
// CHECK:       var Yes: Maybe { get }

// CHECK:       struct BinaryNumbers : Equatable, RawRepresentable {
// CHECK-NEXT:    init(_ rawValue: Bool)
// CHECK-NEXT:    init(rawValue: Bool)
// CHECK-NEXT:	  var rawValue: Bool
// CHECK-NEXT:    typealias RawValue = Bool
// CHECK-NEXT:  }

// CHECK:       var One: BinaryNumbers { get }
// CHECK:       var Zero: BinaryNumbers { get }
// CHECK:       struct EnumClass : Equatable, RawRepresentable {
// CHECK-NEXT:    init(_ rawValue: Bool)
// CHECK-NEXT:    init(rawValue: Bool)
// CHECK-NEXT:    var rawValue: Bool
// CHECK-NEXT:    typealias RawValue = Bool
// CHECK-NEXT:  }

// CHECK:       struct WrapperStruct {
// TODO: where is "A" and "B"? They should be member variables.
// CHECK-NEXT:    struct InnerBoolEnum : Equatable, RawRepresentable {
// CHECK-NEXT:      init(_ rawValue: Bool)
// CHECK-NEXT:      init(rawValue: Bool)
// CHECK-NEXT:      var rawValue: Bool
// CHECK-NEXT:      typealias RawValue = Bool
// CHECK-NEXT:    }
// CHECK-NEXT:    init()
// CHECK-NEXT:  }
