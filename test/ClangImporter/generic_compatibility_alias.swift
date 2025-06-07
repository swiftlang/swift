// RUN: %empty-directory(%t)
// RUN: %build-clang-importer-objc-overlays
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -module-name generic_compat_alias -I %S/Inputs/custom-modules -typecheck -verify %s

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import ObjectiveC
import Foundation
import ObjCIRExtras

func foo(_: SwiftConstrGenericNameAlias<String>) {
// expected-error@-1 {{'SwiftConstrGenericNameAlias' requires that 'String' inherit from 'NSNumber'}}
// TODO: validate node in imported Obj-C header.
}

func faz(_: SwiftGenericNameAlias<Int>) {
// expected-error@-1 {{'SwiftGenericNameAlias' requires that 'Int' be a class type}}
// TODO: validate node in imported Obj-C header.
}

func bar(_: SwiftGenericNameAlias<NSNumber>) {} // Ok
func baz<T: AnyObject>(_: SwiftGenericNameAlias<T>) {} // Ok
