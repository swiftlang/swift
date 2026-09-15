// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-ir -o /dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all %s

@com(interface: "26000000-0000-0000-0000-000000000001")
public protocol IWidget {
  func value(_ result: UnsafeMutablePointer<Int32>?) -> Int32
}

@com
open class Widget: IWidget {
  public init() {
  }

  open func value(_ result: UnsafeMutablePointer<Int32>?) -> Int32 {
    result?.pointee = 42
    return 0
  }
}
