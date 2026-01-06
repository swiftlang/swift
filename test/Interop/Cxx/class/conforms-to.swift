// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/conforms-to-imported.swift -module-name ImportedModule -emit-module -emit-module-path %t/ImportedModule.swiftmodule

// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -disable-availability-checking -I %t -I %S/Inputs -module-name SwiftTest -cxx-interoperability-mode=default
// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -disable-availability-checking -I %t -I %S/Inputs -module-name SwiftTest -cxx-interoperability-mode=swift-5.9
// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -disable-availability-checking -I %t -I %S/Inputs -module-name SwiftTest -cxx-interoperability-mode=swift-6
// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -disable-availability-checking -I %t -I %S/Inputs -module-name SwiftTest -cxx-interoperability-mode=upcoming-swift

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
func caller(_ x: DerivedFromHasTest) { callee(x) }
func caller(_ x: DerivedFromDerivedFromHasTest) { callee(x) }
func caller(_ x: DerivedFromDerivedFromHasTestWithDuplicateArg) { callee(x) }

func callee(_ _: Playable) {

}

func caller(_ x: Playable) {
    callee(x)
}

func caller(_ x: MultipleConformanceHasTestAndPlay) {
    callee(x as Testable)
    callee(x as Playable)
}

func caller(_ x: DerivedFromHasPlay) { callee(x) }
func caller(_ x: DerivedFromDerivedFromHasPlay) { callee(x) }
func caller(_ x: DerivedFromMultipleConformanceHasTestAndPlay) {
    callee(x as Testable)
    callee(x as Playable)
}

func caller(_ x: HasTestAndPlay) {
    callee(x as Testable)
    callee(x as Playable)
}

func caller(_ x: DerivedFromHasTestAndPlay) {
    callee(x as Testable)
    callee(x as Playable)
}

func callee(_ _: ProtocolFromImportedModule) {
}

func caller(_ x: HasImportedConf) {
    callee(x)
}
func caller(_ x: DerivedFromHasImportedConf) { callee(x) }
func caller(_ x: DerivedFromDerivedFromHasImportedConf) { callee(x) }
