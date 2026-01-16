// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/retroactive_conformance_other.swiftmodule %S/Inputs/retroactive_conformance_other.swift
// RUN: %target-swift-frontend -emit-ir %s -I %t | %FileCheck %s

import retroactive_conformance_other

public struct Foo<T: P> {}

public enum MyError : Error {
  case a
}

extension E: @retroactive P where First: P {}

public enum Bar<T: P> {
  case x(Foo<E<T, MyError>>)
  case y
}

// rdar://168023786
public struct G<B: Equatable> {}

extension X: @retroactive Equatable {
  public static func ==(_: Self, _: Self) -> Bool { return false }
}

public func blackHole<T>(_: T) {}

// CHECK-LABEL: @"symbolic _____y_____SgACSQHPABSQ23retroactive_conformanceyHC_HCg_Gm 23retroactive_conformance1GV 0a1_B6_other1XV" = {{.*}}"SgACSQHPABSQ23retroactive_conformanceyHC_HCg_Gm"
blackHole(G<Optional<X>>.self)


// Note that the next symbol name is still wrong, because it mentions
// 'Copyable' and 'Escapable', even though the type signature of
// brokenMangling() does not involve any inverse generics.
//
// Swift 6.3 produced the following mangling:
//
// s23retroactive_conformance14brokenManglingyyAA1GVy0a1_B6_other1XVSgAHSQHPAGSQAAyHC_HCg_GF

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s23retroactive_conformance14brokenManglingyyAA1GVy0a1_B6_other1XVSgAHSQHPAGs8CopyableHPyHC_AGSQAAyHCAGs9EscapableHPyHCHCg_GF"()
public func brokenMangling(_: G<Optional<X>>) {}
