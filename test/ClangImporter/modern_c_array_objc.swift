// Test various ObjC-specific features with C arrays.

// REQUIRES: objc_interop
// REQUIRES: concurrency
// REQUIRES: swift_feature_ModernImportedCArrays
// REQUIRES: swift_feature_ModernImportedCArraysOnly
// REQUIRES: swift_feature_ObjCAttrAllowsCArrays

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -verify-ignore-unrelated %t/typecheck.swift -I %t/include -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -verify-additional-prefix modern-c-array- -enable-experimental-feature ObjCAttrAllowsCArrays
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %t/representability.swift -I %t/include -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -parse-as-library -enable-experimental-feature ObjCAttrAllowsCArrays
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil %t/codegen.swift -I %t/include -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -DMODERN_C_ARRAYS -o /dev/null -enable-experimental-feature ObjCAttrAllowsCArrays

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -verify-ignore-unrelated %t/typecheck.swift -I %t/include -verify-additional-prefix legacy-c-array- -enable-experimental-feature ObjCAttrAllowsCArrays
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %t/representability.swift -I %t/include -parse-as-library -enable-experimental-feature ObjCAttrAllowsCArrays
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil %t/codegen.swift -I %t/include -o /dev/null -enable-experimental-feature ObjCAttrAllowsCArrays

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %t/disabled.swift -I %t/include

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -verify-ignore-unrelated %t/availability.swift -I %t/include -enable-experimental-feature ModernImportedCArraysOnly

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -module-to-print=CLib -source-filename=x -I %t/include --access-filter-private -enable-experimental-feature ModernImportedCArraysOnly -target %target-has-inline-array-triple | %FileCheck %s

//--- include/module.modulemap
module CLib {
  header "CLib.h"
  export *
}

//--- include/CLib.h
#import <Foundation.h>

// The mock SDK doesn't define these.
#define NS_ASSUME_NONNULL_BEGIN _Pragma("clang assume_nonnull begin")
#define NS_ASSUME_NONNULL_END _Pragma("clang assume_nonnull end")

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
@optional
- (void)optionalTakes:(int (*)[4])array;
- (nullable int (*)[4])optionalGives;
@property (nullable, nonatomic) int (*optionalBuf)[4];
@end

@interface Adopter : NSObject <P>
@end

typedef int NTArray[4] __attribute__((swift_wrapper(struct)));

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

// CHECK-LABEL: class Table
// CHECK: @available({{.*}}, *)
// CHECK-NEXT: {{.*}}subscript(index: UInt) -> UnsafeMutablePointer<[4 of CInt]>
@interface Table : NSObject
- (int (*)[4])objectAtIndexedSubscript:(NSUInteger)index;
- (void)setObject:(int (*)[4])object atIndexedSubscript:(NSUInteger)index;
@end

// Declared availability that is lower than InlineArray's is raised to it, on
// the storage as well as its accessors.
// (anyAppleOS can't express a version below InlineArray's.)
#define OLD_AVAILABILITY \
    __attribute__((availability(macos, introduced=12.0))) \
    __attribute__((availability(ios, introduced=15.0))) \
    __attribute__((availability(tvos, introduced=15.0))) \
    __attribute__((availability(watchos, introduced=8.0))) \
    __attribute__((availability(visionos, introduced=1.0)))
@interface OldAvailability : NSObject
@property (nonatomic) int (*row)[4] OLD_AVAILABILITY;
@property (nonatomic, readonly) int (*readonlyRow)[4] OLD_AVAILABILITY;
@property (nonatomic, readonly) int (*redeclaredRow)[4] OLD_AVAILABILITY;
- (int (*)[4])objectAtIndexedSubscript:(NSUInteger)index OLD_AVAILABILITY;
- (void)setObject:(int (*)[4])object
    atIndexedSubscript:(NSUInteger)index OLD_AVAILABILITY;
@end

@interface OldAvailability ()
@property (nonatomic, readwrite) int (*redeclaredRow)[4] OLD_AVAILABILITY;
@end

// The availability of a requirement of a protocol without availability is
// inferred from its types, and that has to include the types of its modern
// projection as well as InlineArray's.
__attribute__((availability(anyappleos, introduced=30.0)))
@interface FutureObject : NSObject
@end

// CHECK-LABEL: protocol InferredAvailability
// CHECK: @available({{.*}}30.0{{.*}}, *)
// CHECK-NEXT: func take(_ array: UnsafeMutablePointer<[4 of CInt]>?, future: FutureObject?)
// CHECK: @available({{.*}}, *)
// CHECK-NEXT: subscript(index: UInt) -> UnsafeMutablePointer<[4 of CInt]>
@protocol InferredAvailability
- (void)takeArray:(int (*_Nullable)[4])array future:(nullable FutureObject *)future;
- (int (*)[4])objectAtIndexedSubscript:(NSUInteger)index;
- (void)setObject:(int (*)[4])object atIndexedSubscript:(NSUInteger)index;
@end

@interface InferredAdopter : NSObject <InferredAvailability>
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

// Conforming from Swift

@available(anyAppleOS 26, *)
class ModernWitness: NSObject, P {
  // expected-legacy-c-array-error@-1 {{type 'ModernWitness' does not conform to protocol 'P'}}
  // expected-legacy-c-array-note@-2 {{add stubs for conformance}}
  func protoTakes(_ array: UnsafeMutablePointer<InlineArray<4, Int32>>) {}
  // expected-legacy-c-array-note@-1 {{candidate has non-matching type '(UnsafeMutablePointer<InlineArray<4, Int32>>) -> ()'}}
  func optionalTakes(_ array: UnsafeMutablePointer<InlineArray<4, Int32>>) {}
}

@available(anyAppleOS 26, *)
class LegacyWitness: NSObject, P {
  // expected-modern-c-array-error@-1 {{type 'LegacyWitness' does not conform to protocol 'P'}}
  // expected-modern-c-array-note@-2 {{add stubs for conformance}}
  func protoTakes(_ array: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>) {}
  // expected-modern-c-array-note@-1 {{candidate has non-matching type '(UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>) -> ()'}}
  func optionalTakes(_ array: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>) {}
}

// Trying to implement the hidden projection, not the visible one, of the
// optional requirement is diagnosed as a conflict.

@available(anyAppleOS 26, *)
class ModernRequiredLegacyOptionalWitness: NSObject, P {
  // expected-legacy-c-array-error@-1 {{type 'ModernRequiredLegacyOptionalWitness' does not conform to protocol 'P'}}
  // expected-legacy-c-array-note@-2 {{add stubs for conformance}}
  func protoTakes(_ array: UnsafeMutablePointer<InlineArray<4, Int32>>) {}
  // expected-legacy-c-array-note@-1 {{candidate has non-matching type '(UnsafeMutablePointer<InlineArray<4, Int32>>) -> ()'}}
  func optionalTakes(_ array: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>) {}
  // expected-modern-c-array-error@-1 {{Objective-C method 'optionalTakes:' provided by method 'optionalTakes' conflicts with optional requirement method 'optionalTakes' in protocol 'P'}}
  // expected-modern-c-array-note@-7 {{class 'ModernRequiredLegacyOptionalWitness' declares conformance to protocol 'P' here}}
  // expected-modern-c-array-note@-3 {{add '@nonobjc' to silence this error}}
}

@available(anyAppleOS 26, *)
class LegacyRequiredModernOptionalWitness: NSObject, P {
  // expected-modern-c-array-error@-1 {{type 'LegacyRequiredModernOptionalWitness' does not conform to protocol 'P'}}
  // expected-modern-c-array-note@-2 {{add stubs for conformance}}
  func protoTakes(_ array: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>) {}
  // expected-modern-c-array-note@-1 {{candidate has non-matching type '(UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>) -> ()'}}
  func optionalTakes(_ array: UnsafeMutablePointer<InlineArray<4, Int32>>) {}
  // expected-legacy-c-array-error@-1 {{Objective-C method 'optionalTakes:' provided by method 'optionalTakes' conflicts with optional requirement method 'optionalTakes' in protocol 'P'}}
  // expected-legacy-c-array-note@-7 {{class 'LegacyRequiredModernOptionalWitness' declares conformance to protocol 'P' here}}
  // expected-legacy-c-array-note@-3 {{add '@nonobjc' to silence this error}}
}

@available(anyAppleOS 26, *)
func testMirroredInitAndSubscript(
    tuple: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>,
    array: UnsafeMutablePointer<InlineArray<4, Int32>>,
    table: Table
) {
  _ = RAdopter(buf: tuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to expected argument type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  _ = RAdopter(buf: array)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to expected argument type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, Int32>' and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}

  _ = table[0]

  table[1] = tuple
  // expected-modern-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  table[1] = array
  // expected-legacy-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, Int32>' and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}

  _ = table.objectAtIndexedSubscript(0)
  // expected-error@-1 {{'objectAtIndexedSubscript' is unavailable: use subscripting}}
  table.setObject(tuple, atIndexedSubscript: 1)
  // expected-legacy-c-array-error@-1 {{'setObject(_:atIndexedSubscript:)' is unavailable: use subscripting}}
  // expected-modern-c-array-error@-2 {{cannot convert value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to expected argument type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>')}}
  // expected-modern-c-array-note@-3 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  table.setObject(array, atIndexedSubscript: 1)
  // expected-modern-c-array-error@-1 {{'setObject(_:atIndexedSubscript:)' is unavailable: use subscripting}}
  // expected-legacy-c-array-error@-2 {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to expected argument type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>')}}
  // expected-legacy-c-array-note@-3 {{arguments to generic parameter 'Pointee' ('InlineArray<4, Int32>' and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}
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

//--- availability.swift
import CLib

// The availability of the modern projection is the declared availability
// limited by InlineArray's, even when that is declared on an accessor.

func testOldAvailability(_ obj: OldAvailability) {
  // expected-note@-1 12 {{add '@available' attribute to enclosing global function}}
  _ = obj.row
  // expected-error@-1 {{'row' is only available in}} expected-note@-1 {{add 'if #available' version check}}
  obj.row = obj.row
  // expected-error@-1 2 {{'row' is only available in}} expected-note@-1 2 {{add 'if #available' version check}}
  _ = obj.readonlyRow
  // expected-error@-1 {{'readonlyRow' is only available in}} expected-note@-1 {{add 'if #available' version check}}
  _ = obj.redeclaredRow
  // expected-error@-1 {{'redeclaredRow' is only available in}} expected-note@-1 {{add 'if #available' version check}}
  obj.redeclaredRow = obj.row
  // expected-error@-1 {{'redeclaredRow' is only available in}} expected-note@-1 {{add 'if #available' version check}}
  // expected-error@-2 {{'row' is only available in}} expected-note@-2 {{add 'if #available' version check}}

  _ = obj[0]
  // expected-error@-1 {{'subscript(_:)' is only available in}} expected-note@-1 {{add 'if #available' version check}}
  // expected-error@-2 {{getter for 'subscript(_:)' is only available in}} expected-note@-2 {{add 'if #available' version check}}
  obj[1] = obj.row
  // expected-error@-1 {{'subscript(_:)' is only available in}} expected-note@-1 {{add 'if #available' version check}}
  // expected-error@-2 {{setter for 'subscript(_:)' is only available in}} expected-note@-2 {{add 'if #available' version check}}
  // expected-error@-3 {{'row' is only available in}} expected-note@-3 {{add 'if #available' version check}}
}

func testInferredAvailability(_ adopter: InferredAdopter, _ existential: any InferredAvailability) {
  // expected-note@-1 3 {{add '@available' attribute to enclosing global function}}
  adopter.take(nil, future: nil)
  // expected-error@-1 {{ 30.0 or newer}} expected-note@-1 {{add 'if #available' version check}}
  existential.take(nil, future: nil)
  // expected-error@-1 {{ 30.0 or newer}} expected-note@-1 {{add 'if #available' version check}}
  _ = existential[0]
  // expected-error@-1 {{'subscript(_:)' is only available in}} expected-note@-1 {{add 'if #available' version check}}
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
func testMirroredInitAndSubscript(
    _ pointer: UnsafeMutablePointer<CArray>, _ table: Table
) {
  _ = RAdopter(buf: pointer)

  _ = table[0]
  table[1] = pointer
}

#if MODERN_C_ARRAYS
typealias Witness = InlineArray<4, Int32>
#else
typealias Witness = (Int32, Int32, Int32, Int32)
#endif

// Conforming from Swift and using the class

@available(anyAppleOS 26, *)
class Conformer: NSObject, P {
  func protoTakes(_ array: UnsafeMutablePointer<Witness>) {}
  func optionalTakes(_ array: UnsafeMutablePointer<Witness>) {}
  func optionalGives() -> UnsafeMutablePointer<Witness>? { nil }
  var optionalBuf: UnsafeMutablePointer<Witness>?
}

@available(anyAppleOS 26, *)
func testConformer(_ conformer: Conformer, _ pointer: UnsafeMutablePointer<Witness>) {
  conformer.protoTakes(pointer)
  let existential: any P = conformer
  existential.protoTakes(pointer)
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

//--- representability.swift
import CLib

// Both projections of a C array are representable, since they're two spellings
// of the same C type, so every case is written with each of them.
typealias Tuple2<T> = (T, T)
typealias Tuple3<T> = (T, T, T)
typealias Tuple4<T> = (T, T, T, T)
@available(anyAppleOS 26, *)
typealias Inline2<T> = InlineArray<2, T>
@available(anyAppleOS 26, *)
typealias Inline3<T> = InlineArray<3, T>
@available(anyAppleOS 26, *)
typealias Inline4<T> = InlineArray<4, T>

// Pointers to C arrays are representable...

@available(anyAppleOS 26, *)
class Pointers: NSObject {
  @objc func arrayTuple(_ a: UnsafeMutablePointer<Tuple4<Int32>>) {}
  @objc func constPointerTuple(_ a: UnsafePointer<Tuple4<Int32>>?) {}
  @objc func nestedTuple(_ a: UnsafeMutablePointer<Tuple3<Tuple2<Int32>>>) {}
  @objc func resultTuple() -> UnsafeMutablePointer<Tuple4<Int32>>? { nil }
  @objc var propertyTuple: UnsafeMutablePointer<Tuple4<Int32>>?
  @objc func arrayInline(_ a: UnsafeMutablePointer<Inline4<Int32>>) {}
  @objc func constPointerInline(_ a: UnsafePointer<Inline4<Int32>>?) {}
  @objc func nestedInline(_ a: UnsafeMutablePointer<Inline3<Inline2<Int32>>>) {}
  @objc func resultInline() -> UnsafeMutablePointer<Inline4<Int32>>? { nil }
  @objc var propertyInline: UnsafeMutablePointer<Inline4<Int32>>?
  @objc func newtype(_ a: UnsafeMutablePointer<NTArray>) {}
}

@available(anyAppleOS 26, *)
@c func cFunctionTuple(_ a: UnsafeMutablePointer<Tuple4<Int32>>) {}
@available(anyAppleOS 26, *)
@c func cFunctionInline(_ a: UnsafeMutablePointer<Inline4<Int32>>) {}
@available(anyAppleOS 26, *)
@c func cNewtype(_ a: UnsafePointer<NTArray>) {}

// ...but C arrays, and newtypes of them, aren't.

@available(anyAppleOS 26, *)
class Values: NSObject {
  @objc func arrayTuple(_ a: Tuple4<Int32>) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{tuples cannot be represented in Objective-C}}
  @objc func arrayInline(_ a: Inline4<Int32>) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
  @objc func newtype(_ a: NTArray) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
  @objc func mixedTuple(_ a: UnsafeMutablePointer<(Int32, Int64)>) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
  @objc func unrepresentableElementTuple(_ a: UnsafeMutablePointer<Tuple4<String>>) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
  @objc func unrepresentableElementInline(_ a: UnsafeMutablePointer<Inline4<String>>) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}

  // ClangImporter never labels the elements of a tuple it imports, at any level.
  @objc func labeled(_ a: UnsafeMutablePointer<(x: Int32, y: Int32)>) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
  @objc func labeledElements(_ a: UnsafeMutablePointer<((x: Int32, y: Int32), (x: Int32, y: Int32))>) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}

  // A zero-length array isn't valid C, especially in nested position.
  @objc func zeroLength(_ a: UnsafeMutablePointer<InlineArray<0, Int32>>) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
}

@available(anyAppleOS 26, *)
@c func cValueTuple(_ a: Tuple4<Int32>) {}
// expected-error@-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note@-2 {{tuples cannot be represented in C}}
@available(anyAppleOS 26, *)
@c func cValueInline(_ a: Inline4<Int32>) {}
// expected-error@-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note@-2 {{Swift structs cannot be represented in C}}

@available(anyAppleOS 26, *)
@c func cZeroLength(_ a: UnsafeMutablePointer<InlineArray<0, Int32>>) {}
// expected-error@-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note@-2 {{Swift structs cannot be represented in C}}

// The element type has to be representable too.

struct SwiftStruct { var x: Int }
class SwiftClass {}
@objc enum ObjCEnum: Int32 { case a }
@c enum CEnum: Int32 { case a }

@available(anyAppleOS 26, *)
class Elements: NSObject {
  @objc func swiftStructTuple(_ a: UnsafeMutablePointer<Tuple4<SwiftStruct>>) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
  @objc func swiftClassTuple(_ a: UnsafeMutablePointer<Tuple4<SwiftClass>>) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
  @objc func optionalTuple(_ a: UnsafeMutablePointer<Tuple4<Int32?>>) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
  @objc func nestedTuple(_ a: UnsafeMutablePointer<Tuple3<Tuple2<String>>>) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
  @objc func swiftStructInline(_ a: UnsafeMutablePointer<Inline4<SwiftStruct>>) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
  @objc func swiftClassInline(_ a: UnsafeMutablePointer<Inline4<SwiftClass>>) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
  @objc func optionalInline(_ a: UnsafeMutablePointer<Inline4<Int32?>>) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
  @objc func nestedInline(_ a: UnsafeMutablePointer<Inline3<Inline2<String>>>) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}

  // An @objc enum is representable in Objective-C, but not in C.
  @objc func objcEnumTuple(_ a: UnsafeMutablePointer<Tuple4<ObjCEnum>>) {}
  @objc func cEnumTuple(_ a: UnsafeMutablePointer<Tuple4<CEnum>>) {}
  @objc func pointerTuple(_ a: UnsafeMutablePointer<Tuple4<UnsafeMutablePointer<Int32>>>) {}
  @objc func objcEnumInline(_ a: UnsafeMutablePointer<Inline4<ObjCEnum>>) {}
  @objc func cEnumInline(_ a: UnsafeMutablePointer<Inline4<CEnum>>) {}
  @objc func pointerInline(_ a: UnsafeMutablePointer<Inline4<UnsafeMutablePointer<Int32>>>) {}

  // ARC can't infer an ownership qualifier for an array of objects or blocks,
  // though it can for a pointer to one.
  @objc func objectPointee(_ a: UnsafeMutablePointer<NSObject>) {}
  @objc func objectElementTuple(_ a: UnsafeMutablePointer<Tuple4<NSObject>>) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
  @objc func blockElementTuple(_ a: UnsafeMutablePointer<Tuple4<@convention(block) () -> Void>>) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
  @objc func objectElementInline(_ a: UnsafeMutablePointer<Inline4<NSObject>>) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
  @objc func blockElementInline(_ a: UnsafeMutablePointer<Inline4<@convention(block) () -> Void>>) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
}

@available(anyAppleOS 26, *)
@c func cSwiftStructTuple(_ a: UnsafeMutablePointer<Tuple4<SwiftStruct>>) {}
// expected-error@-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note@-2 {{Swift structs cannot be represented in C}}
@available(anyAppleOS 26, *)
@c func cSwiftClassTuple(_ a: UnsafeMutablePointer<Tuple4<SwiftClass>>) {}
// expected-error@-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note@-2 {{Swift structs cannot be represented in C}}
@available(anyAppleOS 26, *)
@c func cObjCEnumTuple(_ a: UnsafeMutablePointer<Tuple4<ObjCEnum>>) {}
// expected-error@-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note@-2 {{Swift structs cannot be represented in C}}
// expected-note@-3 {{use '@objc' to expose this function to Objective-C}}
@available(anyAppleOS 26, *)
@c func cCEnumTuple(_ a: UnsafeMutablePointer<Tuple4<CEnum>>) {}
@available(anyAppleOS 26, *)
@c func cPointerTuple(_ a: UnsafeMutablePointer<Tuple4<UnsafeMutablePointer<Int32>>>) {}
@available(anyAppleOS 26, *)
@c func cSwiftStructInline(_ a: UnsafeMutablePointer<Inline4<SwiftStruct>>) {}
// expected-error@-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note@-2 {{Swift structs cannot be represented in C}}
@available(anyAppleOS 26, *)
@c func cSwiftClassInline(_ a: UnsafeMutablePointer<Inline4<SwiftClass>>) {}
// expected-error@-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note@-2 {{Swift structs cannot be represented in C}}
@available(anyAppleOS 26, *)
@c func cObjCEnumInline(_ a: UnsafeMutablePointer<Inline4<ObjCEnum>>) {}
// expected-error@-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note@-2 {{Swift structs cannot be represented in C}}
// expected-note@-3 {{use '@objc' to expose this function to Objective-C}}
@available(anyAppleOS 26, *)
@c func cCEnumInline(_ a: UnsafeMutablePointer<Inline4<CEnum>>) {}
@available(anyAppleOS 26, *)
@c func cPointerInline(_ a: UnsafeMutablePointer<Inline4<UnsafeMutablePointer<Int32>>>) {}

//--- disabled.swift

// Compiled without any C array feature, so C array types are unrepresentable.

import CLib

@available(anyAppleOS 26, *)
class Pointers: NSObject {
  @objc func tuple(_ a: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}

  @objc func array(_ a: UnsafeMutablePointer<InlineArray<4, Int32>>) {}
  // expected-error@-1 {{instance method cannot be marked '@objc' because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{Swift structs cannot be represented in Objective-C}}
}

@c func cFunctionTuple(_ a: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>) {}
// expected-error@-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note@-2 {{Swift structs cannot be represented in C}}

@available(anyAppleOS 26, *)
@c func cFunctionArray(_ a: UnsafeMutablePointer<InlineArray<4, Int32>>) {}
// expected-error@-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note@-2 {{Swift structs cannot be represented in C}}

// C arrays in `@objcMembers` should just make the method `@nonobjc`.
@available(anyAppleOS 26, *)
@objcMembers class Inferred: NSObject {
  func processTuple(_ p: UnsafeMutablePointer<(Float, Float)>) {}
  func processArray(_ p: UnsafeMutablePointer<InlineArray<2, Float>>) {}
  func processElement(_ p: UnsafeMutablePointer<Float>) {}

  func checkSelectors() {
    _ = #selector(processTuple(_:))
    // expected-error@-1 {{argument of '#selector' refers to instance method 'processTuple' that is not exposed to Objective-C}}
    // expected-note@-7 {{add '@objc' to expose this instance method to Objective-C}}
    _ = #selector(processArray(_:))
    // expected-error@-1 {{argument of '#selector' refers to instance method 'processArray' that is not exposed to Objective-C}}
    // expected-note@-9 {{add '@objc' to expose this instance method to Objective-C}}
    _ = #selector(processElement(_:))
  }
}
