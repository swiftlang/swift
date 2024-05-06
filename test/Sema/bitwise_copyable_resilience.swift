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
// RUN:     -I %t

//--- Library.swift
public enum Oopsional<T> {
case someone(T)
case nobody
}

@frozen public enum Woopsional<T> {
case somebody(T)
case noone
}

public struct Integer {
  var value: Int
}

//--- Downstream.swift
import Library

func take<T: BitwiseCopyable>(_ t: T) {}

struct S_Explicit_With_Oopsional<T> : BitwiseCopyable {
  var o: Oopsional<T> // expected-error{{non_bitwise_copyable_type_member}}
}

func passOopsional<T>(_ t: Oopsional<T>) { take(t) } // expected-error   {{type_does_not_conform_decl_owner}}
                                                     // expected-note@-7 {{where_requirement_failure_one_subst}}


struct S_Explicit_With_Woopsional<T> : BitwiseCopyable {
  var o: Woopsional<T> // expected-error{{non_bitwise_copyable_type_member}}
}

func passWoopsional<T>(_ t: Woopsional<T>) { take(t) } // expected-error    {{type_does_not_conform_decl_owner}}
                                                       // expected-note@-15 {{where_requirement_failure_one_subst}}

extension Integer : @retroactive BitwiseCopyable {} // expected-error {{bitwise_copyable_outside_module}}
