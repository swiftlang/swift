// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/swiftMod.swift -module-name SwiftMod -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/swiftMod.h -I %t -enable-experimental-cxx-interop -Xcc -DFIRSTPASS -enable-experimental-feature CxxExistentialInterop

// RUN: %target-interop-build-swift %t/swiftMod.swift -o %t/swift-execution -module-name SwiftMod -I %t -g -DSECOND_PASS -Xcc -DSWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR

// RUN: %target-codesign %t/swift-execution
// RUN: %target-run %t/swift-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxExistentialInterop

// UNSUPPORTED: OS=windows-msvc

//--- header.h
#ifndef FIRSTPASS

#include "swiftMod.h"

// --- Opaque existential round-trips ---

inline SwiftMod::Drawable createDrawable() {
    return SwiftMod::Drawable(SwiftMod::Circle::init(10));
}

inline void passDrawable(const SwiftMod::Drawable& d) {
    (void)d.draw();
}

inline SwiftMod::Drawable passThroughDrawable(const SwiftMod::Drawable& d) {
    auto copy = d;
    return copy;
}

// --- Class-bound existential round-trips ---

inline SwiftMod::Renderable createRenderable() {
    return SwiftMod::Renderable(SwiftMod::Canvas::init(42));
}

inline void passRenderable(const SwiftMod::Renderable& r) {
    (void)r.render();
}

inline SwiftMod::Renderable passThroughRenderable(const SwiftMod::Renderable& r) {
    auto copy = r;
    return copy;
}

// --- PAT round-trips ---

inline SwiftMod::Container<swift::Int> createIntContainer() {
    return SwiftMod::Container<swift::Int>(SwiftMod::IntArray::init(3));
}

inline SwiftMod::Container<SwiftMod::Drawable> createDrawableContainer() {
    return SwiftMod::Container<SwiftMod::Drawable>(
        SwiftMod::DrawableArray::init(5));
}

#endif

//--- module.modulemap
module SwiftToCxxTest {
    header "header.h"
    requires cplusplus
}

//--- swiftMod.swift
import SwiftToCxxTest

public protocol Drawable {
    func draw() -> Int
}

public protocol Renderable: AnyObject {
    func render() -> Int
}

public protocol Container<Element> {
    associatedtype Element
    func count() -> Int
}

public struct Circle: Drawable {
    var radius: Int
    public init(_ radius: Int) { self.radius = radius }
    public func draw() -> Int { return radius * radius }
}

public class Canvas: Renderable {
    var size: Int
    public init(_ size: Int) { self.size = size }
    public func render() -> Int { return size * 2 }
}

public struct IntArray: Container {
    public typealias Element = Int
    var n: Int
    public init(_ n: Int) { self.n = n }
    public func count() -> Int { return n }
}

public struct DrawableArray: Container {
    public typealias Element = any Drawable
    var n: Int
    public init(_ n: Int) { self.n = n }
    public func count() -> Int { return n }
}

#if SECOND_PASS

func testOpaqueRoundTrip() {
    let d = createDrawable()
    print("draw: \(d.draw())")

    passDrawable(d)
    print("passDrawable ok")

    let d2 = passThroughDrawable(d)
    print("passThrough draw: \(d2.draw())")
}

func testClassBoundRoundTrip() {
    let r = createRenderable()
    print("render: \(r.render())")

    passRenderable(r)
    print("passRenderable ok")

    let r2 = passThroughRenderable(r)
    print("passThrough render: \(r2.render())")
}

func testPATRoundTrip() {
    let c = createIntContainer()
    print("int count: \(c.count())")

    let dc = createDrawableContainer()
    print("drawable count: \(dc.count())")
}

testOpaqueRoundTrip()
testClassBoundRoundTrip()
testPATRoundTrip()

#endif

// CHECK: draw: 100
// CHECK-NEXT: passDrawable ok
// CHECK-NEXT: passThrough draw: 100
// CHECK-NEXT: render: 84
// CHECK-NEXT: passRenderable ok
// CHECK-NEXT: passThrough render: 84
// CHECK-NEXT: int count: 3
// CHECK-NEXT: drawable count: 5

// expected-no-diagnostics
