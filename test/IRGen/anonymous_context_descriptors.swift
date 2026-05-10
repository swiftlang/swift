// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend                           \
// RUN:     %t/C.swift                                   \
// RUN:     -emit-module                                 \
// RUN:     -enable-library-evolution                    \
// RUN:     -module-name C                               \
// RUN:     -emit-module-path %t/C.swiftmodule

// RUN: %target-swift-frontend                           \
// RUN:     %t/A.swift                                   \
// RUN:     -enable-anonymous-context-mangled-names      \
// RUN:     -I %t -emit-ir | %FileCheck %s

// REQUIRES: PTRSIZE=64
// REQUIRES: OS=macosx || OS=ios
// UNSUPPORTED: CPU=arm64e

//--- A.swift
import C
public func callStuff()  {
  var x: any P = Empty()
  if #available(iOS 17.3, macOS 14.3, *) {
    x = MyBuilder.f(Empty())
    x = MyBuilder.h(Empty())
  }
  print(x)
}

// Make sure that when we generate an anoynmous context descriptor its parent
// relative reference to another module uses an indirect relative reference.

// CHECK: @"$s1C9MyBuilderVMn" = external global %swift.type_descriptor
// CHECK: @"got.$s1C9MyBuilderVMn" = private unnamed_addr constant ptr @"$s1C9MyBuilderVMn"
// CHECK: @"$s1C9MyBuilderV1fyQrAA1P_pFZMXX" = linkonce_odr hidden constant <{ i32, i32, i32 }> <{ i32 {{[0-9]+}}, i32 add (i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s1C9MyBuilderVMn"
// CHECK: @"$s1C9MyBuilderV1hyQrAA1P_pFZMXX" = linkonce_odr hidden constant <{ i32, i32, i32 }> <{ i32 {{[0-9]+}}, i32 add (i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s1C9MyBuilderVMn"

//--- C.swift
public protocol P {
  func g()
}

public struct Empty : P {
  public init() {}
  public func g() {}
}

@available(iOS 17.3, macOS 14.3, *)
public struct S : P {
  public init<T: P>(_ c: T) {}

  public func g() {
    print("g")
  }
}

public struct MyBuilder {
  @_alwaysEmitIntoClient
  @available(iOS 17.3, macOS 14.3, *)
  public static func f(_ content: any P) -> some P {
    if #unavailable(iOS 17.3, macOS 14.3) {
      return Empty()
    }
    return S(content)
  }
}


extension MyBuilder {
  @_alwaysEmitIntoClient
  @available(iOS 17.3, macOS 14.3, *)
  public static func h(_ content: any P) -> some P {
    if #unavailable(iOS 17.3, macOS 14.3) {
      return Empty()
    }
    return S(content)
  }
}
