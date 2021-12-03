// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target x86_64-unknown-windows-msvc -parse-as-library -parse-stdlib -emit-module -emit-module-path %t/module.swiftmodule -module-name module -DMODULE %s
// RUN: %target-swift-frontend -target x86_64-unknown-windows-msvc -parse-as-library -parse-stdlib -S -emit-ir %s -module-name main -o - -I%t | %FileCheck %s -check-prefix CHECK-SHARED

#if MODULE

public struct S {}
public var value: S {
  S()
}

public func f(_ s: S) {}

public protocol P {
}

public enum E: P {
}

#else

import module

protocol Q: P {
}

extension E: Q {
}

@main
struct Entry {
  public static func main() {
    f(value)
  }
}

#endif


// Ensure that shared linking does mark the functions as being indirected
// through the IAT.

// CHECK-SHARED:       @"$s6module1EO4main1QADWP" = hidden constant [2 x i8*] [
// CHECK-SHARED-SAME:      i8* bitcast ({ i32, i32, i32, i32, i16, i16, i32, i32 }* @"$s6module1EO4main1QADMc" to i8*),
// CHECK-SHARED-SAME:      i8* null
// CHECK-SHARED-SAME:  ]

// CHECK-SHARED: declare dllimport swiftcc void @"$s6module5valueAA1SVvg"()

// CHECK-SHARED: declare dllimport swiftcc void @"$s6module1fyyAA1SVF"()
