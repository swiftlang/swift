// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// REQUIRES: OS=windows-msvc

// RUN: %target-swiftc_driver -emit-module -emit-module-path %t/swift/dynamic.swiftmodule -I %t/swift %t/dynamic.swift
// RUN: %target-swiftc_driver -static -emit-module -emit-module-path %t/swift/static.swiftmodule -I %t/swift %t/static.swift
// RUN: %target-swiftc_driver -O -emit-ir -I %t/swift %t/library.swift | %FileCheck %s

// CHECK: declare dllimport swiftcc i{{[0-9]+}} @"$s7dynamic1fSiyF"()

//--- dynamic.swift
public func f() -> Int { 32 }

//--- static.swift
import dynamic

@inlinable
public func g() -> Int { f() + 1 }

//--- library.swift
import `static`
public func h() -> Int { g() + 1 }
