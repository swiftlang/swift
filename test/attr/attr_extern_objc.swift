// RUN: %target-typecheck-verify-swift -enable-experimental-feature Extern -disable-availability-checking

// REQUIRES: swift_feature_Extern
// REQUIRES: objc_interop

// AnyClass is representable in C: it maps to the Objective-C runtime type
// 'Class', matching how ClangImporter imports C 'Class' back into Swift.
@_extern(c)
func externAnyClassParam(_: AnyClass)

@_extern(c)
func externAnyClassReturn() -> AnyClass

// Protocol-qualified class metatypes (P.Type, i.e. C's 'Class<P>') are not
// representable: only bare AnyClass is.
@objc protocol EP {}

@_extern(c)
func externProtoMetatype(_: EP.Type) // expected-error {{cannot be represented in C}}
