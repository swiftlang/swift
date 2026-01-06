// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Functions -verify -clang-header-expose-decls=has-expose-attr -disable-availability-checking -typecheck -verify -emit-clang-header-path %t/functions.h

// RUN: cat %s | grep -v _expose > %t/clean.swift
// RUN: %target-swift-frontend %t/clean.swift -module-name Functions -clang-header-expose-decls=all-public -disable-availability-checking -typecheck -verify -emit-clang-header-path %t/header.h
// RUN: %FileCheck %s < %t/header.h

// REQUIRES: objc_interop

// CHECK-NOT: Unsupported
// CHECK: supported

import Foundation

public func supported() {}

@objc
@_expose(Cxx) // expected-error {{'@objc' class 'UnsupportedClass' can not yet be exposed to C++}}
public class UnsupportedClass: NSObject {
    override public init() {
        x = 0
    }

    let x: Int
}
