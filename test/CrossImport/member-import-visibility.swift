// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/lib-templates/* %t/
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify -enable-cross-import-overlays \
// RUN:   %t/OnlyDeclaring.swift \
// RUN:   %t/OnlyBystanding.swift \
// RUN:   %t/NeitherDeclaringNorBystanding.swift \
// RUN:   %t/BothDeclaringAndBystanding.swift \
// RUN:   -I %t/include -I %t/lib/swift -F %t/Frameworks \
// RUN:   -enable-upcoming-feature MemberImportVisibility

// REQUIRES: swift_feature_MemberImportVisibility

//--- OnlyDeclaring.swift

import DeclaringLibrary
// expected-note 2 {{add import of module 'BystandingLibrary'}}

private func test() {
  returnsDeclaringTy().overlayMember() // expected-error {{instance method 'overlayMember()' is not available due to missing import of defining module 'BystandingLibrary'}}
  returnsBystandingTy().overlayMember() // expected-error {{instance method 'overlayMember()' is not available due to missing import of defining module 'BystandingLibrary'}}
}

//--- OnlyBystanding.swift

import BystandingLibrary
// expected-note 2 {{add import of module 'DeclaringLibrary'}}

private func test() {
  returnsDeclaringTy().overlayMember() // expected-error {{instance method 'overlayMember()' is not available due to missing import of defining module 'DeclaringLibrary'}}
  returnsBystandingTy().overlayMember() // expected-error {{instance method 'overlayMember()' is not available due to missing import of defining module 'DeclaringLibrary'}}
}

//--- NeitherDeclaringNorBystanding.swift

import Swift
// expected-note 2 {{add import of module 'BystandingLibrary'}}
// expected-note@-1 2 {{add import of module 'DeclaringLibrary'}}

private func test() {
  returnsDeclaringTy().overlayMember() // expected-error {{instance method 'overlayMember()' is not available due to missing imports of defining modules 'BystandingLibrary' and 'DeclaringLibrary'}}
  returnsBystandingTy().overlayMember() // expected-error {{instance method 'overlayMember()' is not available due to missing imports of defining modules 'BystandingLibrary' and 'DeclaringLibrary'}}
}

//--- BothDeclaringAndBystanding.swift

import DeclaringLibrary
import BystandingLibrary

func returnsDeclaringTy() -> DeclaringLibraryTy {
  return DeclaringLibraryTy()
}

func returnsBystandingTy() -> BystandingLibraryTy {
  return BystandingLibraryTy()
}
