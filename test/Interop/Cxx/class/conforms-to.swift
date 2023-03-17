// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/conforms-to-imported.swift -module-name ImportedModule -emit-module -emit-module-path %t/ImportedModule.swiftmodule

// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %t -I %S/Inputs -module-name SwiftTest -enable-experimental-cxx-interop

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

func callee(_ _: Playable) {

}

func caller(_ x: Playable) {
    callee(x)
}

func callee(_ _: ProtocolFromImportedModule) {
}

func caller(_ x: HasImportedConf) {
    callee(x)
}
