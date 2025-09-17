// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -cxx-interoperability-mode=default -typecheck -verify -I %t/Inputs %t/test.swift
// RUN: %target-swift-frontend -cxx-interoperability-mode=default -Xcc -std=c++20 -verify-additional-prefix cpp20- -D CPP20 -typecheck -verify -I %t/Inputs %t/test.swift

//--- Inputs/module.modulemap
module Test {
    header "noncopyable.h"
    requires cplusplus
}

//--- Inputs/noncopyable.h
#include <string>

struct NonCopyable {
    NonCopyable() = default;
    NonCopyable(const NonCopyable& other) = delete;
    NonCopyable(NonCopyable&& other) = default;
};

template <typename T>
struct OwnsT {
    T element;
    OwnsT() {}
    OwnsT(const OwnsT &other) : element(other.element) {}
    OwnsT(OwnsT&& other) {}
};

using OwnsNonCopyable = OwnsT<NonCopyable>;

#if __cplusplus >= 202002L
template <typename T>
struct RequiresCopyableT {
    T element;
    RequiresCopyableT() {}
    RequiresCopyableT(const RequiresCopyableT &other) requires std::is_copy_constructible_v<T> : element(other.element) {}
    RequiresCopyableT(RequiresCopyableT&& other) {}
};

using NonCopyableRequires = RequiresCopyableT<NonCopyable>;

#endif

//--- test.swift
import Test
import CxxStdlib

func takeCopyable<T: Copyable>(_ x: T) {} // expected-note * {{'where T: Copyable' is implicit here}}

func userDefinedTypes() {
    let nCop = NonCopyable()
    takeCopyable(nCop) // expected-error {{global function 'takeCopyable' requires that 'NonCopyable' conform to 'Copyable'}}

    let ownsT = OwnsNonCopyable()
    takeCopyable(ownsT) // no error, OwnsNonCopyable imported as Copyable
}

#if CPP20
func useOfRequires() {
    let nCop = NonCopyableRequires()
    takeCopyable(nCop) // expected-cpp20-error {{global function 'takeCopyable' requires that 'NonCopyableRequires' (aka 'RequiresCopyableT<NonCopyable>') conform to 'Copyable'}}
}
#endif
