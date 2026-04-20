// RUN: %empty-directory(%t) 
// RUN: split-file %s %t

// RUN: %target-build-swift -parse-as-library -enable-library-evolution -emit-module -emit-module-path=%t/Enum.swiftmodule -module-name=Enum %t/enum.swift -c -o %t/enum.o
// RUN: %target-build-swift -module-name=Main -I %t %t/main.swift -c -o %t/main.o
// RUN: %target-swiftc_driver %t/main.o %t/enum.o -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

//--- enum.swift

public struct Payload {
  public let a: Int?
  public let b: Int?
  public let c: Int?
  public let d: Int?
}

public enum MyEnum {
  case big(Payload)
  case small(String)
}

public extension MyEnum {
  @inlinable var isSmall: Bool {
    if case .small = self {
      return true
    }
    return false
  }

  @inlinable var smallValue: String? {
    if case .small(let t) = self {
      return t
    }
    return nil
  }
}

//--- main.swift

import Enum

func main() {
  let v: MyEnum = .small("hello")
  _ = v.isSmall
  let smallValue = v.smallValue

  precondition(smallValue == "hello")

  // CHECK: ok
  print("ok")
}

main()

