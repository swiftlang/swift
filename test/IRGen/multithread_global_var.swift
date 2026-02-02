// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend %t/src/A.swift %t/src/B.swift -emit-ir -o %t/A.ll -o %t/B.ll -num-threads 2 -O -g -module-name test
// RUN: %FileCheck --check-prefix=CHECK-A %s <%t/A.ll
// RUN: %FileCheck --check-prefix=CHECK-B %s <%t/B.ll

//--- A.swift

public func f() -> String { "hello" }

public func g() -> Bool {
  f() == X.introduction
}

// CHECK-A: @"$s4test1XV12introduction_Wz" = external hidden global

//--- B.swift

public struct X {
  public static var introduction: String = f().uppercased()
}

// CHECK-B: @"$s4test1XV12introduction_Wz" = weak_odr hidden global
