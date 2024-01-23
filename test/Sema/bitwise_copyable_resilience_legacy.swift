// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend                           \
// RUN:     %t/Library.swift                             \
// RUN:     -emit-module                                 \
// RUN:     -enable-library-evolution                    \
// RUN:     -module-name Library                         \
// RUN:     -emit-module-path %t/Library.swiftmodule

// RUN: %target-swift-frontend                           \
// RUN:     %t/Downstream.swift                          \
// RUN:     -typecheck -verify                           \
// RUN:     -debug-diagnostic-names                      \
// RUN:     -enable-experimental-feature BitwiseCopyable \
// RUN:     -I %t

//--- Library.swift
@frozen public struct FlexibleBox {
  var t: Int
}

//--- Downstream.swift
import Library

func take<T: _BitwiseCopyable>(_ t: T) {}

struct Rebox : _BitwiseCopyable {
  var b: FlexibleBox // expected-error {{non_bitwise_copyable_type_member}}
}

func passTupleFlexibleBox(_ t: (FlexibleBox, FlexibleBox)) { take(t) } // expected-error   {{type_does_not_conform_decl_owner}}
                                                                       // expected-note@-7 {{where_requirement_failure_one_subst}}

func passFlexibleBox(_ b: FlexibleBox) { take(b) } // expected-error    {{type_does_not_conform_decl_owner}}

