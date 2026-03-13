// RUN: %empty-directory(%t/src)
// RUN: %empty-directory(%t/sdk)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend -emit-module %t/src/Test.swift \
// RUN:   -module-name Test -swift-version 5 -enable-library-evolution \
// RUN:   -emit-module-path %t/Test.swiftmodule

// RUN: %target-swift-frontend -typecheck %t/src/main.swift \
// RUN:   -module-name main -I %t -verify

// REQUIRES: swift_swift_parser
// REQUIRES: observation

//--- Test.swift

public protocol ObservableConvertibleType {
  associatedtype Element
}

public protocol ObservableType : ObservableConvertibleType {}

public class Observable<Element> : ObservableType {
}

extension ObservableType {
  public static func empty() -> Observable<Element> { fatalError() }
}

//--- main.swift
import Test
import Observation

extension Observable {
  func test() -> Observable<Bool> {
    return Observable<Bool>.empty()
  }
}
