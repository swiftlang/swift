// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -cxx-interoperability-mode=default -emit-ir -I %swift_src_root/lib/ClangImporter/SwiftBridging -I %t%{fs-sep}Inputs %t%{fs-sep}test.swift -Xcc -fignore-exceptions -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}noncopyable.h -verify-additional-prefix TEST1- -D TEST1
// RUN: %target-swift-frontend -cxx-interoperability-mode=default -emit-ir -I %swift_src_root/lib/ClangImporter/SwiftBridging -I %t%{fs-sep}Inputs %t%{fs-sep}test.swift -Xcc -fignore-exceptions -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}noncopyable.h -verify-additional-prefix TEST2- -D TEST2
// RUN: %target-swift-frontend -cxx-interoperability-mode=default -emit-ir -I %swift_src_root/lib/ClangImporter/SwiftBridging -I %t%{fs-sep}Inputs %t%{fs-sep}test.swift -Xcc -fignore-exceptions -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}noncopyable.h -verify-additional-prefix TEST3- -D TEST3
// RUN: %target-swift-frontend -cxx-interoperability-mode=default -emit-ir -I %swift_src_root/lib/ClangImporter/SwiftBridging -I %t%{fs-sep}Inputs %t%{fs-sep}test.swift -Xcc -fignore-exceptions -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}noncopyable.h -verify-additional-prefix TEST4- -D TEST4 -Xcc -DTEST4 -Xcc -std=c++20
// RUN: %target-swift-frontend -cxx-interoperability-mode=default -emit-ir -I %swift_src_root/lib/ClangImporter/SwiftBridging -I %t%{fs-sep}Inputs %t%{fs-sep}test.swift -Xcc -fignore-exceptions -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}noncopyable.h -verify-additional-prefix TEST5- -D TEST5
// RUN: %target-swift-frontend -cxx-interoperability-mode=default -emit-ir -I %swift_src_root/lib/ClangImporter/SwiftBridging -I %t%{fs-sep}Inputs %t%{fs-sep}test.swift -Xcc -fignore-exceptions -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}noncopyable.h -verify-additional-prefix TEST6- -D TEST6
// RUN: %target-swift-frontend -cxx-interoperability-mode=default -emit-ir -I %swift_src_root/lib/ClangImporter/SwiftBridging -I %t%{fs-sep}Inputs %t%{fs-sep}test.swift -Xcc -fignore-exceptions -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}noncopyable.h -verify-additional-prefix TEST7-%target-os-family- -verify-additional-prefix TEST7-%target-os- -D TEST7 -Xcc -DTEST7 -Xcc -std=c++20 -verify-ignore-unrelated

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
    NonCopyable(int x) : number(x) {}
    NonCopyable(const NonCopyable& other) = delete; 
    // expected-TEST1-note@-1 {{'NonCopyable' has been explicitly marked deleted here}}
    // expected-TEST2-note@-2 {{'NonCopyable' has been explicitly marked deleted here}}
    // expected-TEST3-note@-3 {{'NonCopyable' has been explicitly marked deleted here}}
    // expected-TEST4-note@-4 {{'NonCopyable' has been explicitly marked deleted here}}
    // expected-TEST6-note@-5 {{'NonCopyable' has been explicitly marked deleted here}}
    NonCopyable(NonCopyable&& other) = default;

    int number = 0;
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
    // expected-TEST1-note@-5 {{annotate a type with SWIFT_COPYABLE_IF(<T>) in C++ to specify that the type is Copyable if <T> is Copyable}}
    // expected-TEST7-DARWIN-error@-6 {{call to implicitly-deleted copy constructor of 'std::unique_ptr<int>'}}
    // expected-TEST7-linux-android-error@-7 {{call to implicitly-deleted copy constructor of 'std::unique_ptr<int>'}}
    // expected-TEST7-linux-androideabi-error@-8 {{call to implicitly-deleted copy constructor of 'std::unique_ptr<int>'}}
    // expected-TEST7-linux-gnu-error@-9 {{call to deleted constructor of 'std::unique_ptr<int>'}}
    // expected-TEST7-DARWIN-note@-10 {{in instantiation of member function 'OwnsT<std::unique_ptr<int>>::OwnsT' requested here}}
    // expected-TEST7-LINUX-note@-11 {{in instantiation of member function 'OwnsT<std::unique_ptr<int>>::OwnsT' requested here}}
    OwnsT(OwnsT&& other) {}
};

using OwnsNonCopyable = OwnsT<NonCopyable>;

template <typename T> struct Derived : OwnsT<T> {
    // expected-TEST2-error@-1 {{failed to copy 'Derived<NonCopyable>'; did you mean to import 'Derived<NonCopyable>' as ~Copyable?}}
    // expected-TEST2-note@-2 {{use 'requires' (since C++20) to specify the constraints under which the copy constructor is available}}
    // expected-TEST2-note@-3 {{annotate a type with SWIFT_COPYABLE_IF(<T>) in C++ to specify that the type is Copyable if <T> is Copyable}}
    // expected-TEST2-note@-4 {{annotate a type with SWIFT_NONCOPYABLE in C++ to import it as ~Copyable}}
};

using DerivedNonCopyable = Derived<NonCopyable>;

template <typename T> struct SWIFT_COPYABLE_IF(T) Annotated {
    T element;
    Annotated() : element() {}
    Annotated(const Annotated &other) : element(other.element) {}
    // expected-TEST3-error@-1 {{failed to copy 'Annotated<OwnsT<NonCopyable>>'; did you mean to import 'Annotated<OwnsT<NonCopyable>>' as ~Copyable?}}
    // expected-TEST3-note@-2 {{one of the types that 'Annotated<OwnsT<NonCopyable>>' depends on may need a 'requires' clause (since C++20) in the copy constructor, a SWIFT_COPYABLE_IF annotation or a SWIFT_NONCOPYABLE annotation'}}
    // expected-TEST3-note@-3 {{the SWIFT_COPYABLE_IF annotation on 'Annotated<OwnsT<NonCopyable>>' may be missing a parameter}}

    Annotated(Annotated &&) = default;
};

using AnnotatedOwnsNonCopyable = Annotated<OwnsT<NonCopyable>>;

#if TEST4
template <typename T> struct Requires {
    T element;
    Requires() : element() {}
    Requires(const Requires &other) requires std::is_copy_constructible_v<T> : element(other.element) {}
    // expected-TEST4-error@-1 {{failed to copy 'Requires<OwnsT<NonCopyable>>'; did you mean to import 'Requires<OwnsT<NonCopyable>>' as ~Copyable?}}
    // expected-TEST4-note@-2 {{one of the types that 'Requires<OwnsT<NonCopyable>>' depends on may need a 'requires' clause (since C++20) in the copy constructor, a SWIFT_COPYABLE_IF annotation or a SWIFT_NONCOPYABLE annotation'}}
    // expected-TEST4-note@-3 {{the 'requires' clause on the copy constructor of 'Requires<OwnsT<NonCopyable>>' may be missing a constraint}}

    Requires(Requires &&) = default;
};

using RequiresOwnsNonCopyable = Requires<OwnsT<NonCopyable>>;
#endif

struct HasSubscript {
    NonCopyable &operator[](int idx) { return nc; }
    NonCopyable nc;
};

template <typename T> struct DefaultedCopyConstructor : OwnsT<T> {
    public:
    constexpr DefaultedCopyConstructor() : OwnsT<T>() { }
    constexpr DefaultedCopyConstructor(const DefaultedCopyConstructor&) = default;
    // expected-TEST6-error@-1 {{failed to copy 'DefaultedCopyConstructor<NonCopyable>'; did you mean to import 'DefaultedCopyConstructor<NonCopyable>' as ~Copyable?}}
    // expected-TEST6-note@-2 {{use 'requires' (since C++20) to specify the constraints under which the copy constructor is available}}
    // expected-TEST6-note@-3 {{annotate a type with SWIFT_COPYABLE_IF(<T>) in C++ to specify that the type is Copyable if <T> is Copyable}}
    // expected-TEST6-note@-4 {{annotate a type with SWIFT_NONCOPYABLE in C++ to import it as ~Copyable}}
    // expected-TEST7-DARWIN-error@-5 {{failed to copy 'DefaultedCopyConstructor<std::unique_ptr<int>>'; did you mean to import 'DefaultedCopyConstructor<std::unique_ptr<int>>' as ~Copyable?}}
    // expected-TEST7-LINUX-error@-6 {{failed to copy 'DefaultedCopyConstructor<std::unique_ptr<int>>'; did you mean to import 'DefaultedCopyConstructor<std::unique_ptr<int>>' as ~Copyable?}}
    // expected-TEST7-DARWIN-note@-7 {{use 'requires' (since C++20) to specify the constraints under which the copy constructor is available}}
    // expected-TEST7-LINUX-note@-8 {{use 'requires' (since C++20) to specify the constraints under which the copy constructor is available}}
    // expected-TEST7-DARWIN-note@-9 {{annotate a type with SWIFT_COPYABLE_IF(<T>) in C++ to specify that the type is Copyable if <T> is Copyable}}
    // expected-TEST7-LINUX-note@-10 {{annotate a type with SWIFT_COPYABLE_IF(<T>) in C++ to specify that the type is Copyable if <T> is Copyable}}
    // expected-TEST7-DARWIN-note@-11 {{annotate a type with SWIFT_NONCOPYABLE in C++ to import it as ~Copyable}}
    // expected-TEST7-LINUX-note@-12 {{annotate a type with SWIFT_NONCOPYABLE in C++ to import it as ~Copyable}}

    constexpr DefaultedCopyConstructor(DefaultedCopyConstructor&&) = default;

    ~DefaultedCopyConstructor() = default;
};

using DefaultedCopyConstructorNonCopyable = DefaultedCopyConstructor<NonCopyable>;

#if TEST7
#include <memory>
using DefaultedCopyConstructorUniquePtr = DefaultedCopyConstructor<std::unique_ptr<int>>;
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

#elseif TEST5
func useSubscript() {
    var obj = HasSubscript(nc: NonCopyable(5))
    let _ = obj[42] // expected-TEST5-error {{'obj.subscript' is borrowed and cannot be consumed}}
    // expected-TEST5-note@-1 {{consumed here}}
    
    func borrow(_ x: borrowing NonCopyable) -> Int32 { return x.number; }
    let _ = borrow(obj[42])
}

#elseif TEST6
func derivedWithDefaultedCopyConstructor() {
    let s = DefaultedCopyConstructorNonCopyable()
    takeCopyable(s)
}

#elseif TEST7 && (canImport(Darwin) || os(Android) || os(Linux))
func stdUniquePtr() {
    let s = DefaultedCopyConstructorUniquePtr()
    takeCopyable(s)
}
#endif
