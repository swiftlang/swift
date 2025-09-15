// RUN: %target-swift-emit-ir -target armv7-apple-none-macho -parse-stdlib -module-name Swift %s -enable-experimental-feature Embedded -wmo -parse-as-library | %FileCheck %s --check-prefix CHECK --check-prefix CHECK-NONOPT
// RUN: %target-swift-emit-ir -target armv7-apple-none-macho -parse-stdlib -module-name Swift %s -enable-experimental-feature Embedded -wmo -parse-as-library -O | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: CODEGENERATOR=ARM
// REQUIRES: swift_feature_Embedded

precedencegroup AssignmentPrecedence { assignment: true }
public typealias IntegerLiteralType = Int
public typealias _MaxBuiltinIntegerType = Builtin.IntLiteral
public typealias _MaxBuiltinFloatType = Builtin.FPIEEE80
public protocol _ExpressibleByBuiltinIntegerLiteral {
  init(_builtinIntegerLiteral value: _MaxBuiltinIntegerType)
}
public protocol _ExpressibleByBuiltinFloatLiteral {
  init(_builtinFloatLiteral value: _MaxBuiltinFloatType)
}
public protocol ExpressibleByIntegerLiteral {
  associatedtype IntegerLiteralType : _ExpressibleByBuiltinIntegerLiteral
  init(integerLiteral value: IntegerLiteralType)
}
public protocol ExpressibleByFloatLiteral {
  associatedtype FloatLiteralType : _ExpressibleByBuiltinFloatLiteral
  init(floatLiteral value: FloatLiteralType)
}
public struct Int : _ExpressibleByBuiltinIntegerLiteral, ExpressibleByIntegerLiteral {
  var value: Builtin.Word
  public init() {
    self = 0
  }
  public init(_builtinIntegerLiteral value: _MaxBuiltinIntegerType) {
    let builtinValue = Builtin.s_to_s_checked_trunc_IntLiteral_Word(value).0
    self.value = builtinValue
  }
  public init(integerLiteral value: IntegerLiteralType) {
    self = value
  }
}

private let global_int_1: Int = 0
public let global_int_2: Int = 0
private var global_int_3: Int = 0
public var global_int_4: Int = 0

private enum MyPrivateEnum { }

public enum MyPublicEnum { }

extension MyPrivateEnum {
  private static let static_int_1: Int = 0
  public static let static_int_2: Int = 0
  private static var static_int_3: Int = 0
  public static var static_int_4: Int = 0
}

extension MyPublicEnum {
  private static let static_int_1: Int = 0
  public static let static_int_2: Int = 0
  private static var static_int_3: Int = 0
  public static var static_int_4: Int = 0
}

private struct MyPrivateStruct {
  var x, y: Int
}

private let global_myprivatestruct_1: MyPrivateStruct = MyPrivateStruct(x: 0, y: 0)
private var global_myprivatestruct_3: MyPrivateStruct = MyPrivateStruct(x: 0, y: 0)

extension MyPrivateEnum {
  private static let static_myprivatestruct_1: MyPrivateStruct = MyPrivateStruct(x: 0, y: 0)
  private static var static_myprivatestruct_3: MyPrivateStruct = MyPrivateStruct(x: 0, y: 0)
}

extension MyPublicEnum {
  private static let static_myprivatestruct_1: MyPrivateStruct = MyPrivateStruct(x: 0, y: 0)
  private static var static_myprivatestruct_3: MyPrivateStruct = MyPrivateStruct(x: 0, y: 0)
}

public struct MyPublicStruct {
  var x, y: Int
}

private let global_my_publicstruct_1: MyPublicStruct = MyPublicStruct(x: 0, y: 0)
public let global_my_publicstruct_2: MyPublicStruct = MyPublicStruct(x: 0, y: 0)
private var global_my_publicstruct_3: MyPublicStruct = MyPublicStruct(x: 0, y: 0)
public var global_my_publicstruct_4: MyPublicStruct = MyPublicStruct(x: 0, y: 0)

extension MyPrivateEnum {
  private static let static_mypublicstruct_1: MyPublicStruct = MyPublicStruct(x: 0, y: 0)
  public static let static_mypublicstruct_2: MyPublicStruct = MyPublicStruct(x: 0, y: 0)
  private static var static_mypublicstruct_3: MyPublicStruct = MyPublicStruct(x: 0, y: 0)
  public static var static_mypublicstruct_4: MyPublicStruct = MyPublicStruct(x: 0, y: 0)
}

extension MyPublicEnum {
  private static let static_mypublicstruct_1: MyPublicStruct = MyPublicStruct(x: 0, y: 0)
  public static let static_mypublicstruct_2: MyPublicStruct = MyPublicStruct(x: 0, y: 0)
  private static var static_mypublicstruct_3: MyPublicStruct = MyPublicStruct(x: 0, y: 0)
  public static var static_mypublicstruct_4: MyPublicStruct = MyPublicStruct(x: 0, y: 0)
}

// CHECK-NONOPT: @"$es12global_int_133_056BEF60D619AD2945081A9CBFC2AAE9LLSivp" = {{.*}}zeroinitializer
// CHECK:        @"$es12global_int_2Sivp" = {{.*}}zeroinitializer
// CHECK-NONOPT: @"$es12global_int_333_056BEF60D619AD2945081A9CBFC2AAE9LLSivp" = {{.*}}zeroinitializer
// CHECK:        @"$es12global_int_4Sivp" = {{.*}}zeroinitializer
// CHECK-NONOPT: @"$es12MyPublicEnumO12static_int_133_056BEF60D619AD2945081A9CBFC2AAE9LLSivpZ" = {{.*}}zeroinitializer
// CHECK:        @"$es12MyPublicEnumO12static_int_2SivpZ" = {{.*}}zeroinitializer
// CHECK-NONOPT: @"$es12MyPublicEnumO12static_int_333_056BEF60D619AD2945081A9CBFC2AAE9LLSivpZ" = {{.*}}zeroinitializer
// CHECK:        @"$es12MyPublicEnumO12static_int_4SivpZ" = {{.*}}zeroinitializer
// CHECK-NONOPT: @"$es24global_my_publicstruct_133_056BEF60D619AD2945081A9CBFC2AAE9LLs14MyPublicStructVvp" = {{.*}}zeroinitializer
// CHECK:        @"$es24global_my_publicstruct_2s14MyPublicStructVvp" = {{.*}}zeroinitializer
// CHECK-NONOPT: @"$es24global_my_publicstruct_333_056BEF60D619AD2945081A9CBFC2AAE9LLs14MyPublicStructVvp" = {{.*}}zeroinitializer
// CHECK:        @"$es24global_my_publicstruct_4s14MyPublicStructVvp" = {{.*}}zeroinitializer
// CHECK-NOT:    static_my_publicstruct_1
// CHECK:        @"$es12MyPublicEnumO23static_mypublicstruct_2s0aB6StructVvpZ" = {{.*}}zeroinitializer
// CHECK-NOT:    static_my_publicstruct_3
// CHECK:        @"$es12MyPublicEnumO23static_mypublicstruct_4s0aB6StructVvpZ" = {{.*}}zeroinitializer
