/*
RUN: %empty-directory(%t)
RUN: split-file %s %t

Regression test for a crash in Embedded Swift when emitting stdlib C-interop
typealias anchor DIEs, when the debug info was being emitted after the
TypeConverter had been destroyed. This crash did not manifest for non-embedded
Swift, because types had been converted earlier and hit the cache instead.

RUN: %target-swift-frontend -emit-ir -g %t/main.swift -I %t \
RUN:   -enable-experimental-feature Embedded -o - | %FileCheck %s

CHECK: !DIDerivedType(tag: DW_TAG_typedef, name: "${{[se]}}s4CInta

REQUIRES: swift_in_compiler
REQUIRES: embedded_stdlib
REQUIRES: swift_feature_Embedded
*/

//--- main.swift
import CInteropAliasModule

public func use<T>(_ t: T) {}

public func main() {
  use(42)
}

//--- module.modulemap
module CInteropAliasModule {
  header "cinterop-aliases.h"
  export *
}

//--- cinterop-aliases.h
struct StructWithInt {
  int counter;
};
