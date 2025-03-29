// This test is by no means exhaustive, but attempts to catch any places in the
// implementation of ASTDumper's JSON where it might do an unprotected call to
// some method on AST node that would cause an assertion due to some
// unsatisfied precondition. This is a good place to put regression tests if
// issues are discovered in the future.
//
// It is separate from ast-dump-json-no-crash.swift since we don't want that
// other test to require Obj-C interop.

// REQUIRES: objc_interop
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -target %target-swift-5.9-abi-triple -enable-objc-interop -swift-version 6 -parse-as-library -dump-ast -dump-ast-format json %s -module-name main -o -

import Foundation

@objc public class NotRenamed: NSObject {
    @objc public var property: Int = 0
}

@objc(PFXRenamed) @objcMembers public class Renamed: NSObject {
    @objc(f1) public func f1() {}
    @objc(f2WithInteger:) public func f2(_ x: Int) {}
    @objc(f3withInteger:error:) public func f3(_ x: Int) throws {}

    @objc(objcnested) public var nested: NotRenamed = .init()
}

func f() {
    _ = #selector(Renamed.f1)
    _ = #keyPath(Renamed.nested.property)
}
