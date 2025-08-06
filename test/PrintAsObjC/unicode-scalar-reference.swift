// RUN: %empty-directory(%t)

// Test the behavior of printing Unicode.Scalar in the compatibility header.
// This is wrong, it should either be rejected and considered non-representable
// or actually be printed using a C / Objective-C compatible type.

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   %s -emit-module -verify -o %t -emit-module-doc \
// RUN:   -emit-clang-header-path %t/compat.h
// RUN: %FileCheck %s --input-file %t/compat.h

@_cdecl("referencesScalar")
func referencesScalar() -> Unicode.Scalar { fatalError() }
// CHECK: SWIFT_EXTERN char32_t referencesScalar(void)

@_cdecl("referencesRelated")
func x_referencesRelated(a: CChar32, b: CWideChar) { }
// CHECK: SWIFT_EXTERN void referencesRelated(char32_t a, wchar_t b)

#if !((os(macOS) || targetEnvironment(macCatalyst)) && arch(x86_64))
    // Actual test of Float16, on supported platforms.
    @_cdecl("referencesFloat16")
    func y_referencesFloat16(a: Float16, b: CFloat16) { }
#else
    // Just print the expected string on macOS&x86_64 where there's no Float16.
    // This disables this one check on unsupported platforms.
    /// SWIFT_EXTERN void referencesFloat16(_Float16 a, _Float16 b)
    @_cdecl("printTheDoc")
    public func y_printTheDoc() { }
#endif
// CHECK: SWIFT_EXTERN void referencesFloat16(_Float16 a, _Float16 b)
