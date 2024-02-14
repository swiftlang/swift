// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend                           \
// RUN:     %t/Library.swift                             \
// RUN:     -emit-module                                 \
// RUN:     -package-name Package                        \
// RUN:     -enable-library-evolution                    \
// RUN:     -enable-experimental-feature BitwiseCopyable \
// RUN:     -module-name Library                         \
// RUN:     -emit-module-path %t/Library.swiftmodule     \
// RUN:     -emit-module-interface-path %t/Library.swiftinterface

// RUN: %target-swift-frontend                           \
// RUN:     %t/Downstream.swift                          \
// RUN:     -typecheck -verify                           \
// RUN:     -package-name Package                        \
// RUN:     -debug-diagnostic-names                      \
// RUN:     -enable-experimental-feature BitwiseCopyable \
// RUN:     -I %t

//--- Library.swift

// !public => conforms
package struct PackageStruct {
  package var int: Int
}

// Public => !conforms
public struct PublicStruct {
  public var int: Int
}

//--- Downstream.swift
import Library

func take<T : _BitwiseCopyable>(_ t: T) {}

func passPackageStruct(_ s: PackageStruct) { take(s) }

func passPublicStruct(_ s: PublicStruct) { take(s) } // expected-error{{type_does_not_conform_decl_owner}}
                                                     // expected-note@-5{{where_requirement_failure_one_subst}}
