// RUN: %swift_frontend_plain -target i686-unknown-windows-msvc -disable-objc-interop -parse-as-library -parse-stdlib -module-name Swift -static -O -S -emit-ir -o - -I%S/Inputs/foreign %s | %FileCheck %s -check-prefix CHECK-IR
// RUN: %swift_frontend_plain -target i686-unknown-windows-msvc -disable-objc-interop -parse-as-library -parse-stdlib -module-name Swift -static -O -S -o - -I%S/Inputs/foreign %s | %FileCheck %s -check-prefix CHECK-ASM

import foreign

precedencegroup AssignmentPrecedence {
  assignment: true
}

public typealias _MaxBuiltinIntegerType = Builtin.IntLiteral

public protocol _ExpressibleByBuiltinIntegerLiteral {
  init(_builtinIntegerLiteral value: _MaxBuiltinIntegerType)
}

public protocol ExpressibleByIntegerLiteral {
  associatedtype IntegerLiteralType: _ExpressibleByBuiltinIntegerLiteral
  init(integerLiteral value: IntegerLiteralType)
}

extension ExpressibleByIntegerLiteral where Self: _ExpressibleByBuiltinIntegerLiteral {
  public init(integerLiteral value: Self) {
    self = value
  }
}

public protocol _ExpressibleByBuiltinFloatLiteral {
  init(_builtinFloatLiteral value: Builtin.FPIEEE64)
}

public protocol ExpressibleByFloatLiteral {
  init(floatLiteral value: _ExpressibleByBuiltinFloatLiteral)
}

public struct Int32 {
  var _value: Builtin.Int32
  init(_ _value: Builtin.Int32) {
    self._value = _value
  }
}

extension Int32: _ExpressibleByBuiltinIntegerLiteral {
  public init(_builtinIntegerLiteral value: Builtin.IntLiteral) {
    _value = Builtin.s_to_s_checked_trunc_IntLiteral_Int32(value).0
  }
}

extension Int32: ExpressibleByIntegerLiteral {
}

public typealias CInt = Int32

public func stdcall() -> CInt {
  return stdcall_add(1, 2)
}

// CHECK-IR-LABEL: $ss7stdcalls5Int32VyF
// CHECK-IR: %0 = tail call x86_stdcallcc i32 @"\01_stdcall_add@8"(i32 1, i32 2)
// CHECK-IR: ret i32 %0

// CHECK-ASM-LABEL: _$ss7stdcalls5Int32VyF
// CHECK-ASM: pushl $2
// CHECK-ASM: pushl $1
// CHECK-ASM: calll _stdcall_add@8
// CHECK-ASM-NOT: addl $8, %esp
// CHECK-ASM: retl
