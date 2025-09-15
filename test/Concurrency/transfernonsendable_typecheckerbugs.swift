// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/src)
// RUN: %empty-directory(%t/sdk)
// RUN: %empty-directory(%t/sdk/ObjCAPI)
// RUN: %empty-directory(%t/sdk/SwiftAPI)
// RUN: %empty-directory(%t/compile)
// RUN: split-file %s %t/src

// Build Objective-C lib
// RUN: %target-clang -dynamiclib %t/src/ObjCAPI.m -I %t/src -o %t/sdk/ObjCAPI/libObjCAPI.dylib -lobjc
// RUN: cp %t/src/ObjCAPI.modulemap %t/sdk/ObjCAPI/module.modulemap
// RUN: cp %t/src/ObjCAPI.h %t/sdk/ObjCAPI/ObjCAPI.h

// Build the swift part of API
// RUN: %target-swiftc_driver -emit-library -emit-module-path %t/sdk/SwiftAPI/SwiftAPI.swiftmodule %t/src/SwiftAPI.swift -parse-as-library -I %t/sdk -swift-version 6 -module-name SwiftAPI -enable-library-evolution

// Now compile against the API.
// RUN: %target-swift-frontend -emit-sil -o /dev/null -I %t/sdk/SwiftAPI -I %t/sdk/ObjCAPI %t/src/main.swift -swift-version 6

// REQUIRES: asserts
// REQUIRES: concurrency
// REQUIRES: objc_interop

// Test

//--- ObjCAPI.modulemap

module ObjCAPI {
  header "ObjCAPI.h"
}

//--- ObjCAPI.h

#define NS_SWIFT_SENDABLE __attribute__((swift_attr("@Sendable")))
#define NS_SWIFT_NONSENDABLE __attribute__((swift_attr("@_nonSendable")))

NS_SWIFT_NONSENDABLE
@interface MyParentClass
@end

NS_SWIFT_SENDABLE
@interface MySubClass : MyParentClass
@end

//--- ObjCAPI.m

#include "ObjCAPI.h"

@implementation MyParentClass
@end

@implementation MySubClass
@end

//--- SwiftAPI.swift

@_exported import ObjCAPI // Clang module

public struct Measurement<T : MyParentClass> {
  /// The unit component of the `Measurement`.
  public let unit: T

  public var count: Int { 0 }
}

extension Measurement : Sendable where T : Sendable {}

//--- main.swift

public import SwiftAPI

public enum MarketplaceKitError : Sendable {
  case first
  case second(Measurement<MySubClass>)

  public var description: String {
    switch self {
    case .first:
      return "first"
    case .second(let value):
      return "\(value.count)"
    }
  }
}
