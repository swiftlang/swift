// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -cxx-interoperability-mode=default -emit-ir -I %swift_src_root/lib/ClangImporter/SwiftBridging -I %t%{fs-sep}Inputs %t%{fs-sep}test.swift -Xcc -fignore-exceptions -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}noncopyable.h -verify-additional-prefix TEST1- -D TEST1
// RUN: %target-swift-frontend -cxx-interoperability-mode=default -emit-ir -I %swift_src_root/lib/ClangImporter/SwiftBridging -I %t%{fs-sep}Inputs %t%{fs-sep}test.swift -Xcc -fignore-exceptions -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}noncopyable.h -verify-additional-prefix TEST2- -D TEST2
// RUN: %target-swift-frontend -cxx-interoperability-mode=default -emit-ir -I %swift_src_root/lib/ClangImporter/SwiftBridging -I %t%{fs-sep}Inputs %t%{fs-sep}test.swift -Xcc -fignore-exceptions -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}noncopyable.h -verify-additional-prefix TEST3- -D TEST3
// RUN: %target-swift-frontend -cxx-interoperability-mode=default -emit-ir -I %swift_src_root/lib/ClangImporter/SwiftBridging -I %t%{fs-sep}Inputs %t%{fs-sep}test.swift -Xcc -fignore-exceptions -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}noncopyable.h -verify-additional-prefix TEST4- -D TEST4 -Xcc -std=c++20

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
    NonCopyable(const NonCopyable& other) = delete; // expected-note {{'NonCopyable' has been explicitly marked deleted here}}
    NonCopyable(NonCopyable&& other) = default;
};

template <typename T>
struct OwnsT {
    T element;
    OwnsT() {}
    OwnsT(const OwnsT &other) : element(other.element) {} 
    // expected-error@-1 *{{call to deleted constructor of 'NonCopyable'}}
    // expected-note@-2 *{{in instantiation of member function 'OwnsT<NonCopyable>::OwnsT' requested here}}
    // expected-TEST1-error@-3 {{failed to copy 'OwnsT<NonCopyable>'; did you mean to import 'OwnsT<NonCopyable>' as ~Copyable?}}
    // expected-TEST1-note@-4 {{use 'requires' (since C++20) to specify the constraints under which the copy constructor is available}}
    // expected-TEST1-note@-5 {{annotate a type with 'SWIFT_COPYABLE_IF(<T>)' in C++ to specify that the type is Copyable if <T> is Copyable}}
    OwnsT(OwnsT&& other) {}
};

using OwnsNonCopyable = OwnsT<NonCopyable>;

template <typename T> struct Derived : OwnsT<T> {
    // expected-TEST2-error@-1 {{failed to copy 'Derived<NonCopyable>'; did you mean to import 'Derived<NonCopyable>' as ~Copyable?}}
    // expected-TEST2-note@-2 {{use 'requires' (since C++20) to specify the constraints under which the copy constructor is available}}
    // expected-TEST2-note@-3 {{annotate a type with 'SWIFT_COPYABLE_IF(<T>)' in C++ to specify that the type is Copyable if <T> is Copyable}}
    // expected-TEST2-note@-4 {{annotate a type with 'SWIFT_NONCOPYABLE' in C++ to import it as ~Copyable}}
};

using DerivedNonCopyable = Derived<NonCopyable>;

template <typename T> struct SWIFT_COPYABLE_IF(T) Annotated {
    T element;
    Annotated() : element() {}
    Annotated(const Annotated &other) : element(other.element) {}
    // expected-TEST3-error@-1 {{failed to copy 'Annotated<OwnsT<NonCopyable>>'; did you mean to import 'Annotated<OwnsT<NonCopyable>>' as ~Copyable?}}
    // expected-TEST3-note@-2 {{one of the types that 'Annotated<OwnsT<NonCopyable>>' depends on may need a 'requires' clause (since C++20) in the copy constructor, a 'SWIFT_COPYABLE_IF' annotation or a 'SWIFT_NONCOPYABLE' annotation'}}
    // expected-TEST3-note@-3 {{the 'SWIFT_COPYABLE_IF' annotation on 'Annotated<OwnsT<NonCopyable>>' may be missing a parameter}}

    Annotated(Annotated &&) = default;
};

using AnnotatedOwnsNonCopyable = Annotated<OwnsT<NonCopyable>>;

#if __cplusplus >= 202002L
template <typename T> struct Requires {
    T element;
    Requires() : element() {}
    Requires(const Requires &other) requires std::is_copy_constructible_v<T> : element(other.element) {}
    // expected-TEST4-error@-1 {{failed to copy 'Requires<OwnsT<NonCopyable>>'; did you mean to import 'Requires<OwnsT<NonCopyable>>' as ~Copyable?}}
    // expected-TEST4-note@-2 {{one of the types that 'Requires<OwnsT<NonCopyable>>' depends on may need a 'requires' clause (since C++20) in the copy constructor, a 'SWIFT_COPYABLE_IF' annotation or a 'SWIFT_NONCOPYABLE' annotation'}}
    // expected-TEST4-note@-3 {{the 'requires' clause on the copy constructor of 'Requires<OwnsT<NonCopyable>>' may be missing a constraint}}

    Requires(Requires &&) = default;
};

using RequiresOwnsNonCopyable = Requires<OwnsT<NonCopyable>>;
#endif

//--- test.swift
import Test
import CxxStdlib

func takeCopyable<T: Copyable>(_ x: T) {}

#if TEST1
func simpleTest() {
    let s = OwnsNonCopyable()
    takeCopyable(s) 
}

#elseif TEST2
func derived() {
    let s = DerivedNonCopyable()
    takeCopyable(s) 
}

#elseif TEST3
func annotated() {
    let s = AnnotatedOwnsNonCopyable()
    // Annotated has a correct SWIFT_COPYABLE_IF annotation, but OwnsT does not
    // Since we import OwnsT<NonCopyable> as Copyable (even though it cannot be copy constructible), 
    // we end up also importing Annotated<OwnsT<NonCopyable>> as Copyable
    takeCopyable(s)
}

#elseif TEST4
func requires() {
    let s = RequiresOwnsNonCopyable()
    // Requires makes use of 'requires' correctly, but OwnsT is missing some information
    // Since we import OwnsT<NonCopyable> as Copyable (even though it cannot be copy constructible), 
    // we end up also importing Requires<OwnsT<NonCopyable>> as Copyable
    takeCopyable(s)
}

#endif
