// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend                           \
// RUN:     %t/Library.swift                             \
// RUN:     -emit-module                                 \
// RUN:     -module-name Library                         \
// RUN:     -emit-module-path %t/Library.swiftmodule

// RUN: %target-swift-frontend                           \
// RUN:     %t/Downstream.swift                          \
// RUN:     -typecheck -verify                           \
// RUN:     -debug-diagnostic-names                      \
// RUN:     -I %t

//--- Library.swift
public enum Oopsional<T> {
case someone(Int)
case nobody
}

@frozen public enum Woopsional<T> {
case somebody(Int)
case noone
}

//--- Downstream.swift
import Library

func take<T: BitwiseCopyable>(_ t: T) {}

struct S_Explicit_With_Oopsional<T> : BitwiseCopyable {
  var o: Oopsional<T> // expected-error{{non_bitwise_copyable_type_member}}
}

func passOopsional<T>(_ t: Oopsional<T>) { take(t) } // expected-error{{type_does_not_conform_decl_owner}}
                                                     // expected-note@-7{{where_requirement_failure_one_subst}}


struct S_Explicit_With_Woopsional<T> : BitwiseCopyable {
  var o: Woopsional<T>
}

func passWoopsional<T>(_ t: Woopsional<T>) { take(t) }
