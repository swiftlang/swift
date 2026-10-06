// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -parse-stdlib -target i686-unknown-windows-msvc -module-name Swift -emit-module %t/Swift.swift -o %t/Swift.swiftmodule
// RUN: %target-swift-frontend -parse-stdlib -target i686-unknown-windows-msvc -enable-experimental-com-interop -com-interop-model=microsoft -I %t -module-name Library -emit-module %t/Library.swift -o %t/Library.swiftmodule
// RUN: %target-swift-frontend -parse-stdlib -target i686-unknown-windows-msvc -enable-experimental-com-interop -I %t -module-name Client -emit-ir -O %t/Client.swift | %FileCheck %s
// REQUIRES: CODEGENERATOR=X86

// Preserve the imported callback's calling convention in the library's SIL
// and when that body is deserialized and inlined into the client.
// CHECK-LABEL: define{{.*}} @"$s6Client4call
// CHECK: call x86_stdcallcc i32 %{{.*}}(i32 {{%.*}})
// CHECK-LABEL: define{{.*}} @"$s6Client9callCDecl
// CHECK: call i32 %{{.*}}(i32 {{%.*}})

//--- Swift.swift
public struct Int32 {
  public var _value: Builtin.Int32
}
public typealias CInt = Int32

//--- module.modulemap
module Callbacks {
  header "Callbacks.h"
  export *
}

//--- Callbacks.h
typedef int (__stdcall *Callback)(int);
typedef int (__cdecl *CDeclCallback)(int);

//--- Library.swift
import Swift
import Callbacks

@inlinable
public func invoke(_ callback: Callback, _ value: Int32) -> Int32 {
  callback(value)
}

@inlinable
public func invokeCDecl(_ callback: CDeclCallback, _ value: Int32) -> Int32 {
  callback(value)
}

//--- Client.swift
import Swift
import Callbacks
import Library

public func call(_ callback: Callback, _ value: Int32) -> Int32 {
  invoke(callback, value)
}

public func callCDecl(_ callback: CDeclCallback, _ value: Int32) -> Int32 {
  invokeCDecl(callback, value)
}
