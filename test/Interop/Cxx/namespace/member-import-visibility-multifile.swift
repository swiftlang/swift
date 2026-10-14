// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default -enable-upcoming-feature MemberImportVisibility -I %S/Inputs %t/ImportsDirect.swift %t/ImportsTransitive.swift

// REQUIRES: swift_feature_MemberImportVisibility

// Both modules declare CommonNamespace::NestedNamespace. Each file can name the
// nested namespace through the module it imports, whichever module's
// declaration of the namespace Swift imported.

//--- ImportsDirect.swift
import MembersDirect

// expected-note@-1 {{add import of module 'MembersTransitive'}}
func takesDirect(_ value: CommonNamespace.NestedNamespace.DirectNestedStruct) -> Int32 {
  return value.memberVar
}

extension CommonNamespace.NestedNamespace.DirectNestedStruct {
  func directExtensionMethod() -> Int32 {
    return memberVar
  }
}

extension CommonNamespace.NestedNamespace.TransitiveNestedStruct { // expected-error {{struct 'TransitiveNestedStruct' is not available due to missing import of defining module 'MembersTransitive'}}
}

//--- ImportsTransitive.swift
import MembersTransitive

// expected-note@-1 {{add import of module 'MembersDirect'}}
func takesTransitive(_ value: CommonNamespace.NestedNamespace.TransitiveNestedStruct) -> Bool {
  return value.memberVar
}

extension CommonNamespace.NestedNamespace.TransitiveNestedStruct {
  func transitiveExtensionMethod() -> Bool {
    return memberVar
  }
}

extension CommonNamespace.NestedNamespace {
  static func transitiveHelper() -> Bool {
    return true
  }
}

extension CommonNamespace.NestedNamespace.DirectNestedStruct { // expected-error {{struct 'DirectNestedStruct' is not available due to missing import of defining module 'MembersDirect'}}
}
