// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/conforms-to-imported.swift -module-name ImportedModule -emit-module -emit-module-path %t/ImportedModule.swiftmodule

// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %t -I %S/Inputs -module-name SwiftTest -enable-experimental-cxx-interop
// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %t -I %S/Inputs -module-name SwiftTest -cxx-interoperability-mode=swift-6 -D UPCOMING_SWIFT
// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %t -I %S/Inputs -module-name SwiftTest -cxx-interoperability-mode=upcoming-swift -D UPCOMING_SWIFT

import ConformsTo
import ImportedModule

protocol Testable {
    func test()
}

protocol Playable {
    func play()
}

func callee(_ _: Testable) {

}

func caller(_ x: HasTest) {
    callee(x)
}
#if UPCOMING_SWIFT
func caller(_ x: DerivedFromHasTest) { callee(x) }
func caller(_ x: DerivedFromDerivedFromHasTest) { callee(x) }
func caller(_ x: DerivedFromDerivedFromHasTestWithDuplicateArg) { callee(x) }
#endif

func callee(_ _: Playable) {

}

func caller(_ x: Playable) {
    callee(x)
}
#if UPCOMING_SWIFT
func caller(_ x: DerivedFromHasPlay) { callee(x) }
func caller(_ x: DerivedFromDerivedFromHasPlay) { callee(x) }

func caller(_ x: HasTestAndPlay) {
    callee(x as Testable)
    callee(x as Playable)
}
func caller(_ x: DerivedFromHasTestAndPlay) {
    callee(x as Testable)
    callee(x as Playable)
}
#endif

func callee(_ _: ProtocolFromImportedModule) {
}

func caller(_ x: HasImportedConf) {
    callee(x)
}
#if UPCOMING_SWIFT
func caller(_ x: DerivedFromHasImportedConf) { callee(x) }
func caller(_ x: DerivedFromDerivedFromHasImportedConf) { callee(x) }
#endif
