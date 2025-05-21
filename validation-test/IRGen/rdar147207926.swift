// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend                              \
// RUN:     %t/Library.swift                                \
// RUN:     -emit-module                                    \
// RUN:     -target %target-swift-5.9-abi-triple            \
// RUN:     -enable-library-evolution                       \
// RUN:     -module-name Library                            \
// RUN:     -emit-module-path %t/Library.swiftmodule

// RUN: %target-build-swift                                 \
// RUN:     %t/Downstream.swift                             \
// RUN:     -c                                              \
// RUN:     -target %target-swift-5.9-abi-triple            \
// RUN:     -parse-as-library                               \
// RUN:     -module-name main                               \
// RUN:     -lLibrary                                       \
// RUN:     -I %t                                           \
// RUN:     -o %t/Executable.o

//--- Library.swift

public struct Paq<each T> {
  public var uette: (repeat each T)
}

public class Loq<each T> {
}

// Enums don't take packs yet.

// public enum Orq<each T> {
//   case uette(repeat each T)
//   case uettette(repeat each T, repeat each T)
// }

//--- Downstream.swift

import Library

struct Sleeve<T> {
  var impl: Paq<T>
}

func bin<Moribund>(_ s: consuming Sleeve<Moribund>) {
}

struct Laq<T> {
  var impl: Loq<T>
  var t: T
}

@_silgen_name("bun")
func bun<T>(_ l: consuming Laq<T>) {
}

// Enums don't take packs yet.

// struct Etiq<T> {
//   var impl: Orq<T>
// }
// 
// func bon<Moribund>(_ i: consuming Etiq<Moribund>) {
// }
