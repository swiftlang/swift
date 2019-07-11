// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -emit-module -o %t %S/Inputs/ImplementProtoRenaming.swift

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -I %t -typecheck %s -verify

// REQUIRES: objc_interop

import Foundation
import ProtoRenaming
import ImplementProtoRenaming

// SR-3917: compiler crash when using an "old" name for an imported requirement
class MyGraphViewSubclass : MyGraphView {
	@objc func doSomethingToGraphView(_ view: GraphView) { } // expected-error{{method 'doSomethingToGraphView' with Objective-C selector 'doSomethingToGraphView:' conflicts with method 'doSomething(to:)' from superclass 'MyGraphView' with the same Objective-C selector}}
}
