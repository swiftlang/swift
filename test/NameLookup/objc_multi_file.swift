// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -primary-file %S/Inputs/objc_multi_file_2.swift %s -verify
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -primary-file %s %S/Inputs/objc_multi_file_2.swift -verify

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module %S/Inputs/objc_multi_file_2.swift -DFAKE_UIIMAGE -o %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -module-name main -primary-file %s -I %t -DIMPORT -verify

// REQUIRES: objc_interop

#if IMPORT
import objc_multi_file_2
#endif

import Foundation

class UIImage : NSObject { }

@objc
protocol ImagePresentingView {
  var hidden: Bool { @objc(isHidden) get set }
}

// rdar://problem/19794036
class SubA : SuperA {
  init() {
    super.init(foo: 42)
  }
}

class SubSubB : SubB {
  // okay: don't conflict with hidden initializers.
  init(wibble: String) { super.init(bar: wibble) }
  init(foo: String) { super.init(bar: foo) }
}

class SubSubB2 : SubB {
  // okay: doesn't conflict with hidden initializer
  func initWithWibble(s: String) { }
}

// rdar://problem/19941580
func rdar19941580(foo: Foo) {
  var _: FooProto = foo
}
