// Test various ObjC-specific features with C arrays.

// REQUIRES: objc_interop
// REQUIRES: concurrency
// REQUIRES: swift_feature_ModernImportedCArrays
// REQUIRES: swift_feature_ModernImportedCArraysOnly

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify %t/typecheck.swift -I %t/include -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -verify-additional-prefix modern-c-array-
// RUN: %target-swift-frontend -emit-sil %t/codegen.swift -I %t/include -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -DMODERN_C_ARRAYS -o /dev/null

// RUN: %target-swift-frontend -typecheck -verify %t/typecheck.swift -I %t/include -verify-additional-prefix legacy-c-array-
// RUN: %target-swift-frontend -emit-sil %t/codegen.swift -I %t/include -o /dev/null

// RUN: %target-swift-ide-test -print-module -module-to-print=CLib -source-filename=x -I %t/include --access-filter-private -enable-experimental-feature ModernImportedCArraysOnly -target %target-has-inline-array-triple | %FileCheck %s

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

// A property redeclared by a protocol that the class adopts.
@protocol ReadonlyProto
@property (readonly, nonatomic, null_unspecified) int (*protoBuf)[4];
@end

@protocol ReadwriteProto <ReadonlyProto>
@property (readwrite, nonatomic, null_unspecified) int (*protoBuf)[4];
@end

@interface ProtoRedeclaringAdopter : NSObject <ReadwriteProto>
@end

@protocol ReadonlyProto2
@property (readonly, nonatomic, null_unspecified) int (*protoBuf2)[4];
@end

@protocol ReadwriteProto2
@property (readwrite, nonatomic, null_unspecified) int (*protoBuf2)[4];
@end

@interface ProtoRedeclaringAdopter2 : NSObject <ReadonlyProto2, ReadwriteProto2>
@end

@protocol P
- (void)protoTakes:(int (*)[4])array;
@end

@interface Adopter : NSObject <P>
@end

@protocol Q
@property int (*protoBuf)[4];
@end

@interface QAdopter : NSObject <Q>
@end

@interface Svc : NSObject
- (void)fetchWithBuf:(int (*)[4])buf
   completionHandler:(void (^)(int (*)[4], NSError *_Nullable))handler;
- (void)resultWithCompletionHandler:(void (^)(int (*_Nullable)[4]))handler;
@end

@protocol AP
- (void)pFetchWithBuf:(int (*)[4])buf
    completionHandler:(void (^)(int (*_Nullable)[4]))handler;
@end

@interface SvcP : NSObject <AP>
@end

@protocol R
- (instancetype)initWithBuf:(int (*)[4])buf;
@end

// CHECK-LABEL: class RAdopter
// CHECK: @available({{.*}}, *)
// CHECK-NEXT: {{.*}}init(buf: UnsafeMutablePointer<[4 of CInt]>)
@interface RAdopter : NSObject <R>
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
func useProtocol(
    _ x: ProtoRedeclaringAdopter,
    tuple: UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>,
    array: UnsafeMutablePointer<InlineArray<4, CInt>>
) {
  x.protoBuf = tuple
  // expected-modern-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>') to type 'UnsafeMutablePointer<[4 of CInt]>?' (aka 'Optional<UnsafeMutablePointer<InlineArray<4, Int32>>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)') and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  x.protoBuf = array
  // expected-legacy-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<InlineArray<4, CInt>>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>') to type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>?' (aka 'Optional<UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, CInt>' (aka 'InlineArray<4, Int32>') and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}
}

@available(anyAppleOS 26, *)
func useUnrelatedProtocols(
    _ x: ProtoRedeclaringAdopter2,
    tuple: UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>,
    array: UnsafeMutablePointer<InlineArray<4, CInt>>
) {
  x.protoBuf2 = tuple
  // expected-modern-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>') to type 'UnsafeMutablePointer<[4 of CInt]>?' (aka 'Optional<UnsafeMutablePointer<InlineArray<4, Int32>>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)') and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  x.protoBuf2 = array
  // expected-legacy-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<InlineArray<4, CInt>>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>') to type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>?' (aka 'Optional<UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, CInt>' (aka 'InlineArray<4, Int32>') and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}
}

@available(anyAppleOS 26, *)
func test(
    tuple: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>,
    array: UnsafeMutablePointer<InlineArray<4, Int32>>,
    adopter: Adopter, qAdopter: QAdopter
) {
  adopter.protoTakes(tuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to expected argument type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  adopter.protoTakes(array)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to expected argument type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, Int32>' and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}

  let existential: any P = adopter
  existential.protoTakes(tuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to expected argument type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  existential.protoTakes(array)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to expected argument type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, Int32>' and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}

  qAdopter.protoBuf = tuple
  // expected-modern-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  qAdopter.protoBuf = array
  // expected-legacy-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, Int32>' and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}
}

@available(anyAppleOS 26, *)
func testAsync(
    tuple: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>,
    array: UnsafeMutablePointer<InlineArray<4, Int32>>,
    svc: Svc
) async throws {
  _ = try await svc.fetch(withBuf: tuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to expected argument type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  _ = try await svc.fetch(withBuf: array)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to expected argument type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, Int32>' and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}

  let _: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>? = await svc.result()
  // expected-modern-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<[4 of CInt]>?' (aka 'Optional<UnsafeMutablePointer<InlineArray<4, Int32>>>') to type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>?'}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('[4 of CInt]' (aka 'InlineArray<4, Int32>') and '(Int32, Int32, Int32, Int32)') are expected to be equal}}
  let _: UnsafeMutablePointer<InlineArray<4, Int32>>? = await svc.result()
  // expected-legacy-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>?' (aka 'Optional<UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>>') to type 'UnsafeMutablePointer<InlineArray<4, Int32>>?'}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)') and 'InlineArray<4, Int32>') are expected to be equal}}
}

@available(anyAppleOS 26, *)
func testMirroredInit(
    tuple: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>,
    array: UnsafeMutablePointer<InlineArray<4, Int32>>
) {
  _ = RAdopter(buf: tuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to expected argument type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  _ = RAdopter(buf: array)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to expected argument type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, Int32>' and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}
}

@available(anyAppleOS 26, *)
func testProtocolAsync(
    tuple: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>,
    array: UnsafeMutablePointer<InlineArray<4, Int32>>,
    svcP: SvcP, proto: any AP
) async {
  _ = await svcP.pFetch(withBuf: tuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to expected argument type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  _ = await svcP.pFetch(withBuf: array)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to expected argument type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, Int32>' and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}

  _ = await proto.pFetch(withBuf: tuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to expected argument type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  _ = await proto.pFetch(withBuf: array)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to expected argument type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, Int32>' and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}
}

// The completion handler form is not affected
@available(anyAppleOS 26, *)
func testCompletionHandler(
    tuple: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>,
    array: UnsafeMutablePointer<InlineArray<4, Int32>>,
    svc: Svc
) {
  svc.fetch(withBuf: tuple) { _, _ in }
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to expected argument type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  svc.fetch(withBuf: array) { _, _ in }
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to expected argument type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, Int32>' and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}
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
typealias CArray = InlineArray<4, Int32>
#else
typealias CArray = (Int32, Int32, Int32, Int32)
#endif

@available(anyAppleOS 26, *)
func test(
    _ pointer: UnsafeMutablePointer<CArray>,
    _ adopter: Adopter, _ qAdopter: QAdopter
) {
  adopter.protoTakes(pointer)
  let existential: any P = adopter
  existential.protoTakes(pointer)

  qAdopter.protoBuf = pointer
  let _: UnsafeMutablePointer<CArray>? = qAdopter.protoBuf
}

@available(anyAppleOS 26, *)
func testAsync(
    _ pointer: UnsafeMutablePointer<CArray>,
    _ svc: Svc, _ svcP: SvcP, _ proto: any AP
) async throws {
  // Array in the completion handler's parameters
  let _: UnsafeMutablePointer<CArray> = try await svc.fetch(withBuf: pointer)
  let _: UnsafeMutablePointer<CArray>? = await svc.result()

  // Protocol requirement
  let _: UnsafeMutablePointer<CArray>? = await svcP.pFetch(withBuf: pointer)
  let _: UnsafeMutablePointer<CArray>? = await proto.pFetch(withBuf: pointer)
}

// The completion handler form is not affected
@available(anyAppleOS 26, *)
func testCompletionHandler(_ pointer: UnsafeMutablePointer<CArray>, _ svc: Svc) {
  svc.fetch(withBuf: pointer) { _, _ in }
}

@available(anyAppleOS 26, *)
func testMirroredInit(_ pointer: UnsafeMutablePointer<CArray>) {
  _ = RAdopter(buf: pointer)
}

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
