// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -I %t -emit-ir %s | %FileCheck %s

@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol IBase {
}

@com(interface: "10000000-0000-0000-0000-000000000002")
public protocol IDerived: IBase {
}

@com(interface: "10000000-0000-0000-0000-000000000003")
public protocol IIndependent {
}

// ISwiftObject has a stable physical projection. A refinement chain shares one
// user projection, while an independent interface needs another. The three
// prefix words make the object address point 24 and increase the total
// instance size from 16 to 40 on a 64-bit target.

// CHECK: @"$s{{.*}}10CCOMObjectCMf" ={{.*}} i32 {{[0-9]+}}, i32 24, i32 40, i16
@com
public final class CCOMObject: IDerived, IIndependent {
}

// Non-COM class metadata retains the ordinary zero address point and size.

// CHECK: @"$s{{.*}}6ObjectCMf" ={{.*}} i32 {{[0-9]+}}, i32 0, i32 16, i16
public final class Object {
}
