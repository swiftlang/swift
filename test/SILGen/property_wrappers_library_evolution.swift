// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t -enable-library-evolution %S/Inputs/property_wrapper_defs.swift
// RUN: %target-swift-emit-silgen -primary-file %s -I %t -enable-library-evolution | %FileCheck %s
import property_wrapper_defs

// rdar://problem/55995892
// This is a crash that occurs only with -enable-library-evolution.

public enum E { case a }
struct M { @MyPublished private var e = E.a }

// Ensure that the backing initializer is serialized.
@frozen
public struct StructUsesPublishedAsPrivate {
  public var integer: Int = 17

  // CHECK: sil non_abi [serialized] [ossa] @$s35property_wrappers_library_evolution28StructUsesPublishedAsPrivateV6stringSSvpfP : $@convention(thin) (@owned String) -> @out MyPublished<String>
  @MyPublished var string: String = "Hello"
}
