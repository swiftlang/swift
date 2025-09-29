// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -cxx-interoperability-mode=default -typecheck -verify -I %swift_src_root/lib/ClangImporter/SwiftBridging -I %t%{fs-sep}Inputs %t%{fs-sep}test.swift -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}noncopyable.h
// RUN: %target-swift-frontend -cxx-interoperability-mode=default -Xcc -std=c++20 -verify-additional-prefix cpp20- -D CPP20 -typecheck -verify -I %swift_src_root/lib/ClangImporter/SwiftBridging -I %t%{fs-sep}Inputs %t%{fs-sep}test.swift -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}noncopyable.h

//--- Inputs/module.modulemap
module Test {
    header "noncopyable.h"
    requires cplusplus
}

//--- Inputs/noncopyable.h
#include "swift/bridging"
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

template <typename T>
struct SWIFT_COPYABLE_IF(T) AnnotatedOwnsT {
    T element;
    AnnotatedOwnsT() {}
    AnnotatedOwnsT(const AnnotatedOwnsT &other) : element(other.element) {}
    AnnotatedOwnsT(AnnotatedOwnsT&& other) {}
};

using AnnotatedOwnsNonCopyable = AnnotatedOwnsT<NonCopyable>;

template <typename F, typename S>
struct SWIFT_COPYABLE_IF(F, S) MyPair {
    F first;
    S second;
};

MyPair<int, NonCopyable> p1();
MyPair<int, NonCopyable*> p2();
MyPair<int, OwnsNonCopyable> p3();
MyPair<int, AnnotatedOwnsNonCopyable> p4();
MyPair<int, MyPair<int, NonCopyable>> p5();
MyPair<NonCopyable, int> p6();

#if __cplusplus >= 202002L
template <typename T>
struct RequiresCopyableT {
    T element;
    RequiresCopyableT() {}
    RequiresCopyableT(const RequiresCopyableT &other) requires std::is_copy_constructible_v<T> : element(other.element) {}
    RequiresCopyableT(RequiresCopyableT&& other) {}
};

using NonCopyableRequires = RequiresCopyableT<NonCopyable>;
using CopyableIfRequires = RequiresCopyableT<MyPair<int, NonCopyable>>;

MyPair<int, NonCopyableRequires> p7();

#endif

template<typename T>
struct SWIFT_COPYABLE_IF(T) SWIFT_NONCOPYABLE DoubleAnnotation {};

using DoubleAnnotationInt = DoubleAnnotation<int>;

struct SWIFT_NONCOPYABLE NonCopyableNonMovable { // expected-note {{record 'NonCopyableNonMovable' is not automatically available: it must have a copy/move constructor and a destructor; does this type have reference semantics?}}
    NonCopyableNonMovable() {}
    NonCopyableNonMovable(const NonCopyableNonMovable& other) {}
    NonCopyableNonMovable(NonCopyableNonMovable&& other) = delete;
};


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

func useCopyableIf() {
    takeCopyable(p1()) // expected-error {{global function 'takeCopyable' requires that 'MyPair<CInt, NonCopyable>' conform to 'Copyable'}}
    takeCopyable(p2())

    // p3() -> MyPair<int, OwnsNonCopyable> is imported as Copyable and will cause an error during IRGen.
    // During typecheck we don't produce an error because we're missing an annotation in OwnsT.
    takeCopyable(p3())
    // p4() -> (MyPair<int, AnnotatedOwnsNonCopyable>) is imported as NonCopyable because AnnotatedOwnsT is correctly annotated.
    takeCopyable(p4()) // expected-error {{global function 'takeCopyable' requires that 'MyPair<CInt, AnnotatedOwnsT<NonCopyable>>' conform to 'Copyable'}}

    takeCopyable(p5()) // expected-error {{global function 'takeCopyable' requires that 'MyPair<CInt, MyPair<CInt, NonCopyable>>' conform to 'Copyable'}}
    takeCopyable(p6()) // expected-error {{global function 'takeCopyable' requires that 'MyPair<NonCopyable, CInt>' conform to 'Copyable'}}
}

#if CPP20
func useOfRequires() {
    let a = NonCopyableRequires()
    takeCopyable(a) // expected-cpp20-error {{global function 'takeCopyable' requires that 'NonCopyableRequires' (aka 'RequiresCopyableT<NonCopyable>') conform to 'Copyable'}}

    let b = CopyableIfRequires()
    takeCopyable(b) // expected-cpp20-error {{global function 'takeCopyable' requires that 'CopyableIfRequires' (aka 'RequiresCopyableT<MyPair<CInt, NonCopyable>>') conform to 'Copyable'}}

    takeCopyable(p7()) // expected-cpp20-error {{global function 'takeCopyable' requires that 'MyPair<CInt, RequiresCopyableT<NonCopyable>>' conform to 'Copyable'}}
}
#endif

func doubleAnnotation() {
    let s = DoubleAnnotationInt()
    takeCopyable(s) // expected-error {{global function 'takeCopyable' requires that 'DoubleAnnotationInt' (aka 'DoubleAnnotation<CInt>') conform to 'Copyable'}}
}

func missingLifetimeOperation() {
    let s = NonCopyableNonMovable() // expected-error {{cannot find 'NonCopyableNonMovable' in scope}}
    takeCopyable(s)
}
