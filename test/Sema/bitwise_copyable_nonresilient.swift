// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend                           \
// RUN:     %t/Library.swift                             \
// RUN:     -emit-module                                 \
// RUN:     -enable-experimental-feature BitwiseCopyable \
// RUN:     -module-name Library                         \
// RUN:     -emit-module-path %t/Library.swiftmodule

// RUN: %target-swift-frontend                           \
// RUN:     %t/Downstream.swift                          \
// RUN:     -typecheck -verify                           \
// RUN:     -debug-diagnostic-names                      \
// RUN:     -enable-experimental-feature BitwiseCopyable \
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

func take<T: _BitwiseCopyable>(_ t: T) {}

struct S_Explicit_With_Oopsional<T> : _BitwiseCopyable {
  var o: Oopsional<T>
}

func passOopsional<T>(_ t: Oopsional<T>) { take(t) }


struct S_Explicit_With_Woopsional<T> : _BitwiseCopyable {
  var o: Woopsional<T>
}

func passWoopsional<T>(_ t: Woopsional<T>) { take(t) }

