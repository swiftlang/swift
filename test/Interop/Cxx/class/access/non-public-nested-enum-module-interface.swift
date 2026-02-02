// RUN: split-file %s %t

// RUN: %target-swift-ide-test -print-module -module-to-print=CxxModule -print-access -I %t/Inputs -source-filename=x -enable-experimental-cxx-interop -enable-experimental-feature ImportNonPublicCxxMembers | %FileCheck %s

// REQUIRES: swift_feature_ImportNonPublicCxxMembers

//--- Inputs/module.modulemap
module CxxModule {
    requires cplusplus
    header "cxx-header.h"
}

//--- Inputs/cxx-header.h
#pragma once

class __attribute__((__swift_attr__("private_fileid:main/base.swift")))
Base {
protected:
  enum class Enum { Foo, Bar };
public:
  Enum makeEnum(void) const { return Enum::Bar; }
};

class __attribute__((__swift_attr__("private_fileid:main/derived.swift")))
Derived : public Base {};

// CHECK:      public struct Base {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   private enum Enum : [[ENUM_UNDERLYING_TYPE:Int32|UInt32]] {
// CHECK-NEXT:     private init?(rawValue: [[ENUM_UNDERLYING_TYPE]])
// CHECK-NEXT:     private var rawValue: [[ENUM_UNDERLYING_TYPE]] { get }
// CHECK-NEXT:     private typealias RawValue = [[ENUM_UNDERLYING_TYPE]]
// CHECK-NEXT:     case Foo
// CHECK-NEXT:     case Bar
// CHECK-NEXT:   }
// CHECK-NEXT:   public func makeEnum() -> Base.Enum
// CHECK-NEXT: }

// CHECK-NEXT: public struct Derived {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   private typealias Enum = Base.Enum
// CHECK-NEXT:   public func makeEnum() -> Base.Enum
// CHECK-NEXT: }
