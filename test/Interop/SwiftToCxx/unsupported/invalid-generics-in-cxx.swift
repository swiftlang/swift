// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Generic declarations whose signatures are invalid are left out of the
// header with -experimental-allow-module-with-compiler-errors. Each case is
// compiled separately so that the errors of one don't affect the others.

// RUN: %target-swift-frontend %t/value-generic.swift -module-name Invalid -typecheck -verify -experimental-allow-module-with-compiler-errors -clang-header-expose-decls=all-public -emit-clang-header-path %t/value-generic.h
// RUN: %FileCheck %s --check-prefix=VALUE < %t/value-generic.h
// RUN: %check-interop-cxx-header-in-clang(%t/value-generic.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// RUN: %target-swift-frontend %t/constrained-extension.swift -module-name Invalid -typecheck -verify -experimental-allow-module-with-compiler-errors -clang-header-expose-decls=all-public -emit-clang-header-path %t/constrained-extension.h
// RUN: %FileCheck %s --check-prefix=EXTENSION < %t/constrained-extension.h
// RUN: %check-interop-cxx-header-in-clang(%t/constrained-extension.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// RUN: %target-swift-frontend %t/pack.swift -module-name Invalid -typecheck -verify -experimental-allow-module-with-compiler-errors -clang-header-expose-decls=all-public -emit-clang-header-path %t/pack.h
// RUN: %FileCheck %s --check-prefix=PACK < %t/pack.h
// RUN: %check-interop-cxx-header-in-clang(%t/pack.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

//--- value-generic.swift
@available(SwiftStdlib 6.2, *)
public func valueGenericOfInvalidType<let N: AnyObject>() {} // expected-error {{'AnyObject' is not a supported value type for 'N'}}
// expected-error@-1 {{generic parameter 'N' is not used in function signature}}

// VALUE-NOT: SWIFT_INLINE_THUNK void valueGenericOfInvalidType

//--- constrained-extension.swift
public protocol HasInvalidRequirement where Undefined == Self {} // expected-error {{cannot find type 'Undefined' in scope}}

public struct Box<T> {
  public var x: Int
}

extension Box where T: HasInvalidRequirement {
  public struct Nested {
    public var y: Int
  }
}

// EXTENSION-NOT: class SWIFT_SYMBOL("{{.*}}") Nested

//--- pack.swift
public protocol HasRecursiveAssoc { // expected-error {{cannot build rewrite system for protocol; concrete type nesting limit exceeded}}
  // expected-note@-1 {{failed rewrite rule is}}
  associatedtype A where A == [A]
}

@available(SwiftStdlib 5.9, *)
public func packWithRequirement<each T>(_ x: repeat each T) where repeat each T: HasRecursiveAssoc {}

// PACK-NOT: SWIFT_INLINE_THUNK void packWithRequirement
