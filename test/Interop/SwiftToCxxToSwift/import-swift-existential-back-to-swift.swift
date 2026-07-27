// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/swiftMod.swift -module-name SwiftMod -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/swiftMod.h -I %t -enable-experimental-cxx-interop -Xcc -DFIRSTPASS -enable-experimental-feature CxxExistentialInterop

// RUN: %target-swift-frontend %t/swiftMod.swift -module-name SwiftMod -clang-header-expose-decls=all-public -emit-module -o %t/SwiftMod.swiftmodule -I %t -enable-experimental-cxx-interop -Xcc -DFIRSTPASS -enable-experimental-feature CxxExistentialInterop

// RUN: %target-swift-ide-test -print-module -module-to-print=SwiftMod -module-to-print=SwiftToCxxTest -I %t -source-filename=x -enable-experimental-cxx-interop -Xcc -DSWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR -Xcc -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -enable-experimental-feature CxxExistentialInterop | %FileCheck --check-prefix=INTERFACE %s

// UNSUPPORTED: OS=windows-msvc
// REQUIRES: swift_feature_CxxExistentialInterop

//--- header.h
#ifndef FIRSTPASS
#include "swiftMod.h"

// --- Opaque existential round-trip ---
SwiftMod::Drawable createDrawable();

void passDrawable(const SwiftMod::Drawable& d);

SwiftMod::Drawable bestDrawable(const SwiftMod::Drawable& a,
                                const SwiftMod::Drawable& b);

// INTERFACE: func createDrawable() -> any Drawable
// INTERFACE: func passDrawable(_ d: any Drawable)
// INTERFACE: func bestDrawable(_ a: any Drawable, _ b: any Drawable) -> any Drawable

// --- Class-bound protocol round-trip ---
SwiftMod::Renderable createRenderable();

void passRenderable(const SwiftMod::Renderable& r);

SwiftMod::Renderable passThroughRenderable(const SwiftMod::Renderable& r);

// INTERFACE: func createRenderable() -> any Renderable
// INTERFACE: func passRenderable(_ r: any Renderable)
// INTERFACE: func passThroughRenderable(_ r: any Renderable) -> any Renderable

// --- PAT protocol round-trip ---
SwiftMod::Container<swift::Int> createIntContainer();

void passIntContainer(const SwiftMod::Container<swift::Int>& c);

SwiftMod::Container<swift::Int> firstIntContainer(
    const SwiftMod::Container<swift::Int>& a,
    const SwiftMod::Container<swift::Int>& b);

// Default template arg (swift::Any) => unconstrained existential
SwiftMod::Container<> createAnyContainer();

// Nested existential PAT arg
SwiftMod::Container<SwiftMod::Drawable> createDrawableContainer();

// INTERFACE: func createIntContainer() -> any Container<Int>
// INTERFACE: func passIntContainer(_ c: any Container<Int>)
// INTERFACE: func firstIntContainer(_ a: any Container<Int>, _ b: any Container<Int>) -> any Container<Int>
// INTERFACE: func createAnyContainer() -> any Container
// INTERFACE: func createDrawableContainer() -> any Container<any Drawable>

// --- Composition round-trip ---
SwiftMod::AnyDrawableAndResizable createComposition();

void passComposition(const SwiftMod::AnyDrawableAndResizable& d);

SwiftMod::AnyDrawableAndResizable passThroughComposition(
    const SwiftMod::AnyDrawableAndResizable& a);

// INTERFACE: func createComposition() -> any Drawable & Resizable
// INTERFACE: func passComposition(_ d: any Drawable & Resizable)
// INTERFACE: func passThroughComposition(_ a: any Drawable & Resizable) -> any Drawable & Resizable

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

public protocol Resizable {
    func resize(to factor: Int) -> Bool
}

public protocol Renderable: AnyObject {
    func render() -> Int
}

public protocol Container<Element> {
    associatedtype Element
    func count() -> Int
}

public struct Circle: Drawable, Resizable {
    var radius: Int
    public init(radius: Int) { self.radius = radius }
    public func draw() -> Int { return radius * radius }
    public func resize(to factor: Int) -> Bool { return factor > 0 && factor <= radius }
}

public class Canvas: Renderable {
    var size: Int
    public init(_ size: Int) { self.size = size }
    public func render() -> Int { return size * 2 }
}

public func makeDrawableAndResizable() -> any Drawable & Resizable {
    return Circle(radius: 5)
}

// expected-no-diagnostics
