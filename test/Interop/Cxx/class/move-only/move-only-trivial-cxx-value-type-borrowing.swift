// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-sil -I %t/Inputs %t/test.swift -cxx-interoperability-mode=default -verify -o /dev/null

//--- Inputs/module.modulemap
module Test {
    header "test.h"
    requires cplusplus
}

//--- Inputs/test.h

// A trivial move-only C++ type: deleted copy ctor, defaulted move ctor,
// trivial destructor (only has int members).
struct TrivialNonCopyable {
    int x = 0;
    TrivialNonCopyable() = default;
    TrivialNonCopyable(int x) : x(x) {}
    TrivialNonCopyable(const TrivialNonCopyable &) = delete;
    TrivialNonCopyable(TrivialNonCopyable &&) = default;
};

inline void takeByValue(TrivialNonCopyable t) { (void)t; }

inline int borrowConstRef(const TrivialNonCopyable &t) { return t.x; }

//--- test.swift
import Test

// Passing a borrowed trivial noncopyable C++ value to a function that takes it
// by value should be rejected, because by-value requires a copy.
func testBorrowingPassToByValue(_ t: borrowing TrivialNonCopyable) { // expected-error {{'t' is borrowed and cannot be consumed}}
    takeByValue(t) // expected-note {{consumed here}}
}

// Passing by const-ref should be fine.
func testBorrowingPassToConstRef(_ t: borrowing TrivialNonCopyable) {
    let _ = borrowConstRef(t) // ok
}

// Consuming parameter should be able to pass to by-value.
func testConsumingPassToByValue(_ t: consuming TrivialNonCopyable) {
    takeByValue(t)
}
