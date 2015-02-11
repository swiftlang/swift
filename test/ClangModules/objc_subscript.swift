// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -I %S/Inputs/custom-modules %s -verify

// REQUIRES: objc_interop

import Foundation
import ObjCSubscripts

// rdar://problem/19772357
class KeySubscript1Sub : KeySubscript1 {
  override subscript (str: String!) -> AnyObject! {
    get { return self }
    set { }
  }
}

class KeySubscript2Sub : KeySubscript2 {
  override subscript (str: String) -> AnyObject? {
    get { return self }
    set { }
  }
}

class KeySubscript3Sub : KeySubscript3 {
  override subscript (str: String) -> String? {
    get { return str }
    set { }
  }
}

class KeySubscript4Sub : KeySubscript4 {
  override subscript (str: [AnyObject]) -> String? {
    get { return nil }
    set { }
  }
}
