// Test that ObjC library code using the modern C array projection can be
// inlined into clients that use the legacy projection, and vice versa. Doing
// this requires that both projections be imported (even if one is hidden) and
// that SIL serialization not mix them up.

// REQUIRES: swift_feature_ModernImportedCArrays
// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: concurrency

// Checks for SIL output. Each call goes through the projection of the module
// that wrote it, whichever projection the module being built uses:

// LEGACY-LABEL: sil private @async_Main
// LEGACY: objc_method {{%.*}}, #Adopter.showBuf!foreign : (Adopter) -> (UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>) -> ()
// LEGACY: objc_method {{%.*}}, #P.showBuf!foreign : <Self where Self : P> (Self) -> (UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>) -> ()
// LEGACY: objc_method {{%.*}}, #Svc.fetch!foreign : (Svc) -> (UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>) async -> UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>
// LEGACY: } // end sil function 'async_Main'

// MODERN-LABEL: sil private @async_Main
// MODERN: objc_method {{%.*}}, #Adopter.showBuf!foreign : (Adopter) -> (UnsafeMutablePointer<[4 of Int32]>) -> ()
// MODERN: objc_method {{%.*}}, #P.showBuf!foreign : <Self where Self : P> (Self) -> (UnsafeMutablePointer<[4 of Int32]>) -> ()
// MODERN: objc_method {{%.*}}, #Svc.fetch!foreign : (Svc) -> (UnsafeMutablePointer<[4 of Int32]>) async -> UnsafeMutablePointer<[4 of Int32]>
// MODERN: } // end sil function 'async_Main'

// SIL-LABEL: sil shared {{.*}}@$s9LegacyLib10legacyFuncyyF
// SIL: objc_method {{%.*}}, #Adopter.showBuf!foreign : (Adopter) -> (UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>) -> ()
// SIL: objc_method {{%.*}}, #P.showBuf!foreign : <Self where Self : P> (Self) -> (UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>) -> ()
// SIL: } // end sil function '$s9LegacyLib10legacyFuncyyF'

// SIL-LABEL: sil shared {{.*}}@$s9ModernLib10modernFuncyyF
// SIL: objc_method {{%.*}}, #Adopter.showBuf!foreign : (Adopter) -> (UnsafeMutablePointer<[4 of Int32]>) -> ()
// SIL: objc_method {{%.*}}, #P.showBuf!foreign : <Self where Self : P> (Self) -> (UnsafeMutablePointer<[4 of Int32]>) -> ()
// SIL: } // end sil function '$s9ModernLib10modernFuncyyF'

// SIL-LABEL: sil shared {{.*}}@$s9LegacyLib15legacyAsyncFuncyyYaF
// SIL: objc_method {{%.*}}, #Svc.fetch!foreign : (Svc) -> (UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>) async -> UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>
// SIL: init_block_storage_header {{.*}} type $@convention(block) @Sendable (UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>) -> ()
// SIL: } // end sil function '$s9LegacyLib15legacyAsyncFuncyyYaF'

// SIL-LABEL: sil shared {{.*}}@$s9ModernLib15modernAsyncFuncyyYaF
// SIL: objc_method {{%.*}}, #Svc.fetch!foreign : (Svc) -> (UnsafeMutablePointer<[4 of Int32]>) async -> UnsafeMutablePointer<[4 of Int32]>
// SIL: init_block_storage_header {{.*}} type $@convention(block) @Sendable (UnsafeMutablePointer<InlineArray<4, Int32>>) -> ()
// SIL: } // end sil function '$s9ModernLib15modernAsyncFuncyyYaF'

// RUN: %empty-directory(%t/include)
// RUN: %empty-directory(%t/src)
// RUN: %empty-directory(%t/fragile)
// RUN: split-file %s %t

//--- include/CLib.h

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@protocol P
- (void)showBuf:(int (*)[4])array;
@end

@interface Adopter : NSObject <P>
@end

@interface Svc : NSObject
- (void)fetchWithBuf:(int (*)[4])buf
   completionHandler:(void (^)(int (*)[4]))handler;
@end

NS_ASSUME_NONNULL_END

//--- include/module.modulemap

module CLib {
  header "CLib.h"
  export *
}

//--- src/Adopter.m

// RUN: %target-clang -fobjc-arc -I %t/include -c %t/src/Adopter.m -o %t/fragile/Adopter.o

#import "CLib.h"
#import <stdio.h>

@implementation Adopter
- (void)showBuf:(int (*)[4])array {
  printf("Adopter %d %d %d %d\n", (*array)[0], (*array)[1], (*array)[2], (*array)[3]);
  fflush(stdout);
}
@end

@implementation Svc
- (void)fetchWithBuf:(int (*)[4])buf
   completionHandler:(void (^)(int (*)[4]))handler {
  (*buf)[0] += 1;
  handler(buf);
}
@end

//--- src/LegacyLib.swift

// RUN: %target-swift-frontend %t/src/LegacyLib.swift -I %t/include -c -module-name LegacyLib -o %t/fragile/LegacyLib.o -emit-module-path %t/fragile/LegacyLib.swiftmodule -parse-as-library -module-cache-path %t/module-cache -swift-version 5 -target %target-has-inline-array-triple

import CLib

@export(implementation) public func legacyFunc() {
  var array: (Int32, Int32, Int32, Int32) = (.max, 1, 2, 3)
  let adopter = Adopter()
  adopter.showBuf(&array)
  let proto: any P = adopter
  proto.showBuf(&array)
}

@export(implementation) public func legacyAsyncFunc() async {
  var array: (Int32, Int32, Int32, Int32) = (1, 2, 3, 4)
  let result = await Svc().fetch(withBuf: &array)
  print("Svc", result.pointee.0, result.pointee.1, result.pointee.2, result.pointee.3)
}

//--- src/ModernLib.swift

// RUN: %target-swift-frontend %t/src/ModernLib.swift -I %t/include -c -module-name ModernLib -o %t/fragile/ModernLib.o -emit-module-path %t/fragile/ModernLib.swiftmodule -parse-as-library -module-cache-path %t/module-cache -swift-version 5 -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple

import CLib

@export(implementation) public func modernFunc() {
  var array: InlineArray<4, Int32> = [.max, 1, 2, 3]
  let adopter = Adopter()
  adopter.showBuf(&array)
  let proto: any P = adopter
  proto.showBuf(&array)
}

@export(implementation) public func modernAsyncFunc() async {
  var array: InlineArray<4, Int32> = [1, 2, 3, 4]
  let result = await Svc().fetch(withBuf: &array)
  print("Svc", result.pointee[0], result.pointee[1], result.pointee[2], result.pointee[3])
}

//--- src/LegacyExec.swift

// RUN: %target-build-swift -emit-sil %t/src/LegacyExec.swift -g -o %t/fragile/LegacyExec.sil -I %t/include -I %t/fragile -module-cache-path %t/module-cache -swift-version 5 -target %target-has-inline-array-triple
// RUN: %FileCheck --input-file %t/fragile/LegacyExec.sil --check-prefixes SIL,LEGACY %s
// RUN: %target-build-swift -emit-executable %t/src/LegacyExec.swift %t/fragile/LegacyLib.o %t/fragile/ModernLib.o %t/fragile/Adopter.o -g -o %t/fragile/LegacyExec -I %t/include -I %t/fragile -module-cache-path %t/module-cache -swift-version 5 -target %target-has-inline-array-triple
// RUN: %target-codesign %t/fragile/LegacyExec
// RUN: %target-run %t/fragile/LegacyExec > %t/fragile/LegacyExec.txt
// RUN: %FileCheck --input-file %t/fragile/LegacyExec.txt --check-prefix OUTPUT %s

import CLib
import LegacyLib
import ModernLib

print("legacyFunc:")
legacyFunc()

print("modernFunc:")
modernFunc()

print("main:")
var array: (Int32, Int32, Int32, Int32) = (.max, 1, 2, 3)
let adopter = Adopter()
adopter.showBuf(&array)
let proto: any P = adopter
proto.showBuf(&array)

print("legacyAsyncFunc:")
await legacyAsyncFunc()

print("modernAsyncFunc:")
await modernAsyncFunc()

print("main async:")
var asyncArray: (Int32, Int32, Int32, Int32) = (1, 2, 3, 4)
let asyncResult = await Svc().fetch(withBuf: &asyncArray)
print("Svc", asyncResult.pointee.0, asyncResult.pointee.1, asyncResult.pointee.2, asyncResult.pointee.3)

//--- src/ModernExec.swift

// RUN: %target-build-swift -emit-sil %t/src/ModernExec.swift -g -o %t/fragile/ModernExec.sil -I %t/include -I %t/fragile -module-cache-path %t/module-cache -swift-version 5 -target %target-has-inline-array-triple -enable-experimental-feature ModernImportedCArrays
// RUN: %FileCheck --input-file %t/fragile/ModernExec.sil --check-prefixes SIL,MODERN %s
// RUN: %target-build-swift -emit-executable %t/src/ModernExec.swift %t/fragile/LegacyLib.o %t/fragile/ModernLib.o %t/fragile/Adopter.o -g -o %t/fragile/ModernExec -I %t/include -I %t/fragile -module-cache-path %t/module-cache -swift-version 5 -target %target-has-inline-array-triple -enable-experimental-feature ModernImportedCArrays
// RUN: %target-codesign %t/fragile/ModernExec
// RUN: %target-run %t/fragile/ModernExec > %t/fragile/ModernExec.txt
// RUN: %FileCheck --input-file %t/fragile/ModernExec.txt --check-prefix OUTPUT %s

import CLib
import LegacyLib
import ModernLib

print("legacyFunc:")
legacyFunc()

print("modernFunc:")
modernFunc()

print("main:")
var array: InlineArray<4, Int32> = [.max, 1, 2, 3]
let adopter = Adopter()
adopter.showBuf(&array)
let proto: any P = adopter
proto.showBuf(&array)

print("legacyAsyncFunc:")
await legacyAsyncFunc()

print("modernAsyncFunc:")
await modernAsyncFunc()

print("main async:")
var asyncArray: InlineArray<4, Int32> = [1, 2, 3, 4]
let asyncResult = await Svc().fetch(withBuf: &asyncArray)
print("Svc", asyncResult.pointee[0], asyncResult.pointee[1], asyncResult.pointee[2], asyncResult.pointee[3])

//--- src/Output.txt

// OUTPUT: legacyFunc:
// OUTPUT-NEXT: Adopter 2147483647 1 2 3
// OUTPUT-NEXT: Adopter 2147483647 1 2 3
// OUTPUT-NEXT: modernFunc:
// OUTPUT-NEXT: Adopter 2147483647 1 2 3
// OUTPUT-NEXT: Adopter 2147483647 1 2 3
// OUTPUT-NEXT: main:
// OUTPUT-NEXT: Adopter 2147483647 1 2 3
// OUTPUT-NEXT: Adopter 2147483647 1 2 3
// OUTPUT-NEXT: legacyAsyncFunc:
// OUTPUT-NEXT: Svc 2 2 3 4
// OUTPUT-NEXT: modernAsyncFunc:
// OUTPUT-NEXT: Svc 2 2 3 4
// OUTPUT-NEXT: main async:
// OUTPUT-NEXT: Svc 2 2 3 4
