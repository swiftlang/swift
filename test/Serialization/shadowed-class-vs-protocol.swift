/// Ensure cross references to shadowed decls take into account the shadowing
/// after the custom deserialization filtering.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// REQUIRES: objc_interop

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   %t/SwiftLib.swift -I %t -emit-module-path %t/SwiftLib.swiftmodule
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   -typecheck %t/Client.swift -I %t

//--- module.modulemap
module RootLib {
  header "RootLib.h"
}

module MiddleLib {
  header "MiddleLib.h"
  export *
}

//--- RootLib.h
__attribute__((swift_name("ShadowedProtocol")))
@protocol Shadowed
@end

//--- MiddleLib.h
@import Foundation;
#include "RootLib.h"

@interface Shadowed: NSObject <Shadowed>
@end

//--- SwiftLib.swift
import MiddleLib

public func funcRef() -> Shadowed { fatalError() }

extension Shadowed {
    public func method() -> Shadowed { fatalError() }
}

//--- Client.swift
import SwiftLib

@inlinable
public func bar() {
    let _ = funcRef().method()
}
