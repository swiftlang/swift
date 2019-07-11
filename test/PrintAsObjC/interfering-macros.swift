// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck -emit-objc-header-path %t/interfering-macros.h
// RUN: %check-in-clang -fsyntax-only -Werror %t/interfering-macros.h -D'any=UNWANTED_MACRO_SUBSTITUTION'

// REQUIRES: objc_interop

import Foundation

@objc public class Test : NSObject {
  public var str: String = ""
  public var strongProp: Any?
}
