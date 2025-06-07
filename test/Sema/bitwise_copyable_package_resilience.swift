// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend                           \
// RUN:     %t/Library.swift                             \
// RUN:     -emit-module                                 \
// RUN:     -package-name Package                        \
// RUN:     -enable-library-evolution                    \
// RUN:     -module-name Library                         \
// RUN:     -emit-module-path %t/Library.swiftmodule     \
// RUN:     -emit-module-interface-path %t/Library.swiftinterface

// RUN: %target-swift-frontend                           \
// RUN:     %t/Downstream.swift                          \
// RUN:     -typecheck -verify                           \
// RUN:     -package-name Package                        \
// RUN:     -debug-diagnostic-names                      \
// RUN:     -I %t

//--- Library.swift

// package => exported => !inferred
package struct PackageStruct {
  package var int: Int
}

// public => exported => !inferred
public struct PublicStruct {
  public var int: Int
}

//--- Downstream.swift
import Library

func take<T : BitwiseCopyable>(_ t: T) {}

func passPackageStruct(_ s: PackageStruct) { take(s) } // expected-error{{type_does_not_conform_decl_owner}}
                                                       // expected-note@-3{{where_requirement_failure_one_subst}}

func passPublicStruct(_ s: PublicStruct) { take(s) } // expected-error{{type_does_not_conform_decl_owner}}
                                                     // expected-note@-6{{where_requirement_failure_one_subst}}
