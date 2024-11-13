// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -I %S/Inputs/custom-modules -import-objc-header %S/Inputs/objc_async_tuple_88949633.h -target %target-swift-5.1-abi-triple %s -verify
// REQUIRES: concurrency
// REQUIRES: objc_interop

// rdar://88949633

import Foundation
import ObjCConcurrency

class Foo: NSObject, NSButz {
    func idString(_: NSObject) async -> (Any?, String?) {
        return (nil, nil)
    }
    func idStringID(_: NSObject) async -> (Any?, String?, Any?) {
        return (nil, nil, nil)
    }
    func stringIDString(_: NSObject) async -> (String?, Any?, String?) {
        return (nil, nil, nil)
    }
    func idStringIDString(_: NSObject) async -> (Any?, String?, Any?, String?) {
        return (nil, nil, nil, nil)
    }
}


