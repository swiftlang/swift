// RUN: rm -rf %t && mkdir -p %t

// FIXME: BEGIN -enable-source-import hackaround
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource) -emit-module -o %t %clang-importer-sdk-path/swift-modules/CoreGraphics.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-module -o %t %clang-importer-sdk-path/swift-modules/Foundation.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %S/Inputs/custom-modules -I %t) -emit-module -o %t %S/Inputs/ImplementProtoRenaming.swift

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %S/Inputs/custom-modules -I %t) -typecheck %s -verify

// REQUIRES: objc_interop

import ProtoRenaming
import ImplementProtoRenaming

// SR-3917: compiler crash when using an "old" name for an imported requirement
class MyGraphViewSubclass : MyGraphView {
	func doSomethingToGraphView(_ view: GraphView) { } // expected-error{{method 'doSomethingToGraphView' with Objective-C selector 'doSomethingToGraphView:' conflicts with method 'doSomething(to:)' from superclass 'MyGraphView' with the same Objective-C selector}}
}
