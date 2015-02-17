// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -parse-as-library -primary-file %S/Inputs/objc_multi_file_2.swift %s -verify

// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module %S/Inputs/objc_multi_file_2.swift -DFAKE_UIIMAGE -o %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -module-name main -primary-file %s -I %t -DIMPORT -verify

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
