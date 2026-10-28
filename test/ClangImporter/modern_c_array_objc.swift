// Test various ObjC-specific features with C arrays.

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ModernImportedCArrays

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated %t/typecheck.swift -I %t/include -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -verify-additional-prefix modern-c-array-

// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated %t/typecheck.swift -I %t/include -verify-additional-prefix legacy-c-array-
// RUN: %target-swift-frontend -emit-sil %t/codegen.swift -I %t/include -o /dev/null

//--- include/module.modulemap
module CLib {
  header "CLib.h"
  export *
}

//--- include/CLib.h
#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface Direct : NSObject
- (void)takes:(int (* _Null_unspecified)[4])p;
@property (nonatomic, null_unspecified) int (*prop)[4];
@end

// Redeclaring a property with a C array refines both projections.
@interface Base : NSObject
@property (readonly, nonatomic, null_unspecified) int (*buf)[4];
@property (readonly, nonatomic, null_unspecified) int (*catBuf)[4];
@end

@interface Base ()
@property (readwrite, nonatomic, null_unspecified) int (*buf)[4];
@end

@interface Base (Redeclared)
@property (readwrite, nonatomic, null_unspecified) int (*catBuf)[4];
@end

// Parameters declared as arrays decay to pointers to arrays, however they are
// qualified.
@interface Matrix : NSObject
- (instancetype)initWithIdentifier:(int)identifier
        informationMatrixSymmetric:(nullable float[6][6])matrix;
- (instancetype)initWithSource:(int)source
    informationMatrixSymmetric:(nonnull const float[6][6])matrix;
@end

NS_ASSUME_NONNULL_END

//--- typecheck.swift
import CLib

// Modern projection of a method should not be visible in legacy mode.

@available(anyAppleOS 26.0, *)
func useHiddenProjection(_ c: Direct, _ p: UnsafeMutablePointer<InlineArray<4, CInt>>?) {
  c.takes(p)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<4, CInt>>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>') to expected argument type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, CInt>' (aka 'InlineArray<4, Int32>') and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}
  c.prop = p
  // expected-legacy-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<InlineArray<4, CInt>>?' (aka 'Optional<UnsafeMutablePointer<InlineArray<4, Int32>>>') to type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>?' (aka 'Optional<UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, CInt>' (aka 'InlineArray<4, Int32>') and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}
}

// A redeclared property is settable in both projections.

@available(anyAppleOS 26, *)
func useExtension(
    _ x: Base,
    tuple: UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>,
    array: UnsafeMutablePointer<InlineArray<4, CInt>>
) {
  x.buf = tuple
  // expected-modern-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>') to type 'UnsafeMutablePointer<[4 of CInt]>?' (aka 'Optional<UnsafeMutablePointer<InlineArray<4, Int32>>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)') and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  x.buf = array
  // expected-legacy-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<InlineArray<4, CInt>>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>') to type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>?' (aka 'Optional<UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, CInt>' (aka 'InlineArray<4, Int32>') and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}
}

@available(anyAppleOS 26, *)
func useCategory(
    _ x: Base,
    tuple: UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>,
    array: UnsafeMutablePointer<InlineArray<4, CInt>>
) {
  x.catBuf = tuple
  // expected-modern-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>') to type 'UnsafeMutablePointer<[4 of CInt]>?' (aka 'Optional<UnsafeMutablePointer<InlineArray<4, Int32>>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)') and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  x.catBuf = array
  // expected-legacy-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<InlineArray<4, CInt>>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>') to type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>?' (aka 'Optional<UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, CInt>' (aka 'InlineArray<4, Int32>') and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}
}
@available(anyAppleOS 26, *)
func testDecayedArrayParameters(
    tuple: UnsafeMutablePointer<(Float, Float, Float, Float, Float, Float)>,
    array: UnsafeMutablePointer<InlineArray<6, Float>>,
    constTuple: UnsafePointer<(Float, Float, Float, Float, Float, Float)>,
    constArray: UnsafePointer<InlineArray<6, Float>>
) {
  _ = Matrix(identifier: 0, informationMatrixSymmetric: tuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<(Float /* ... repeated 6 times ... */)>' to expected argument type 'UnsafeMutablePointer<[6 of CFloat]>' (aka 'UnsafeMutablePointer<InlineArray<6, Float>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Float /* ... repeated 6 times ... */)' and '[6 of CFloat]' (aka 'InlineArray<6, Float>')) are expected to be equal}}
  _ = Matrix(identifier: 0, informationMatrixSymmetric: array)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<6, Float>>' to expected argument type 'UnsafeMutablePointer<(CFloat /* ... repeated 6 times ... */)>' (aka 'UnsafeMutablePointer<(Float /* ... repeated 6 times ... */)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<6, Float>' and '(CFloat /* ... repeated 6 times ... */)' (aka '(Float /* ... repeated 6 times ... */)')) are expected to be equal}}

  _ = Matrix(source: 0, informationMatrixSymmetric: constTuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'UnsafePointer<(Float /* ... repeated 6 times ... */)>' to expected argument type 'UnsafePointer<[6 of CFloat]>' (aka 'UnsafePointer<InlineArray<6, Float>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Float /* ... repeated 6 times ... */)' and '[6 of CFloat]' (aka 'InlineArray<6, Float>')) are expected to be equal}}
  _ = Matrix(source: 0, informationMatrixSymmetric: constArray)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'UnsafePointer<InlineArray<6, Float>>' to expected argument type 'UnsafePointer<(CFloat /* ... repeated 6 times ... */)>' (aka 'UnsafePointer<(Float /* ... repeated 6 times ... */)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<6, Float>' and '(CFloat /* ... repeated 6 times ... */)' (aka '(Float /* ... repeated 6 times ... */)')) are expected to be equal}}
}

//--- codegen.swift
import CLib

#if MODERN_C_ARRAYS
typealias Row6 = InlineArray<6, Float>
#else
typealias Row6 = (Float, Float, Float, Float, Float, Float)
#endif

@available(anyAppleOS 26, *)
func testDecayedArrayParameters(
    _ mutable: UnsafeMutablePointer<Row6>, _ immutable: UnsafePointer<Row6>
) {
  _ = Matrix(identifier: 0, informationMatrixSymmetric: mutable)
  _ = Matrix(source: 0, informationMatrixSymmetric: immutable)
}
