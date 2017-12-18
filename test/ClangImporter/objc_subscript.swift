// RUN: %empty-directory(%t)
// RUN: %build-clang-importer-objc-overlays

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-sil -I %S/Inputs/custom-modules %s -verify

// REQUIRES: objc_interop

import ObjCSubscripts

// rdar://problem/19772357
class KeySubscript1Sub : KeySubscript1 {
  override subscript (str: String!) -> Any! {
    get { return self }
    set { }
  }
}

class KeySubscript2Sub : KeySubscript2 {
  override subscript (str: String) -> Any? {
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
  override subscript (str: [Any]) -> String? {
    get { return nil }
    set { }
  }
}

class ConformsToKeySubscriptProto1 : KeySubscriptProto1 {
  @objc subscript (s: String) -> String? {
    return s
  }
}

class ConformsToKeySubscriptProto2 : KeySubscriptProto2 {
  @objc subscript (s: String!) -> String! {
    get { return s }
    set { }
  }
}

func testOverridesWithoutBase(
  o1: KeySubscriptOverrideGetter,
  o2: KeySubscriptOverrideSetter,
  o3: KeySubscriptReversedOverrideGetter,
  o4: KeySubscriptReversedOverrideSetter
) {
  // rdar://problem/36033356 failed specifically when the base class was never
  // subscripted, so please don't mention the base classes here.
  _ = o1["abc"]
  o1["abc"] = "xyz"

  _ = o2["abc"]
  o2["abc"] = "xyz"

  _ = o3["abc"]
  o3["abc"] = "xyz"

  _ = o4["abc"]
  o4["abc"] = "xyz"
}
