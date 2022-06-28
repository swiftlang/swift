// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/mods
// RUN: split-file %s %t

// RUN: %target-swift-frontend -module-name A -emit-module -emit-module-path %t/mods/A.swiftmodule -experimental-allow-module-with-compiler-errors %t/A.swift
// RUN: %target-swift-frontend -module-name B -emit-module -emit-module-path %t/mods/B.swiftmodule -experimental-allow-module-with-compiler-errors -I%t/mods %t/B.swift 2>&1 | %FileCheck %s

//--- A.swift
public struct StructFromA {
  public func badMethod(arg: missing) {}
}

//--- B.swift
import A

public func test() {
  // The init is internal so we expect to get a diagnostic for that. But
  // deserializing `StructFromA` could potentially output a diagnostic for the
  // error type on `field`. Make sure that we don't crash and that there is a
  // diagnostic for the internal init.
  _ = StructFromA()
  // CHECK: error: 'StructFromA' initializer is inaccessible due to 'internal' protection level
}

