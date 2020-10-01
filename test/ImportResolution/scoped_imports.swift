// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -enable-objc-interop -emit-module %S/Inputs/overlay.swift -module-name ClangModuleWithOverlay -I %S/Inputs/ClangModuleWithOverlay -o %t
// RUN: %target-typecheck-verify-swift -I %t -show-diagnostics-after-fatal

// Make sure we can perform a scoped import on the overlay.
import func ClangModuleWithOverlay.fromSwiftOverlay

// ... as well as the underlying Clang module.
import func ClangModuleWithOverlay.fromUnderlyingClang

// We do not currently support scoped imports of type members.
import var ClangModuleWithOverlay.ClangType.instanceMember // expected-error {{no such module 'ClangModuleWithOverlay.ClangType'}}
import func ClangModuleWithOverlay.ClangType.importAsMemberStaticMethod // expected-error {{no such module 'ClangModuleWithOverlay.ClangType'}}
import func ClangModuleWithOverlay.ClangType.importAsMemberInstanceMethod // expected-error {{no such module 'ClangModuleWithOverlay.ClangType'}}
import struct ClangModuleWithOverlay.ClangType.Inner // expected-error {{no such module 'ClangModuleWithOverlay.ClangType'}}

// We currently allow referring to import-as-member decls by their Clang names.
// FIXME: Should we reject this? We don't appear to translate the access path,
// so a lookup for e.g ClangType.Inner will still fail (as shown below).
import func ClangModuleWithOverlay.ClangTypeImportAsMemberStaticMethod
import func ClangModuleWithOverlay.ClangTypeImportAsMemberInstanceMethod
import struct ClangModuleWithOverlay.ClangTypeInner

func foo(_ x: ClangType.Inner) {} // expected-error {{cannot find type 'ClangType' in scope}}
