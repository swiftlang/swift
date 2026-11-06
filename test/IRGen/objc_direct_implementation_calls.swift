// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect
// REQUIRES: swift_feature_ObjCImplementation

// Swift calls a direct member it implements with '@objc @implementation'
// through its native entry point. The header's symbol, e.g. '-[Calc triple]',
// is never defined: Swift names the definition '-[Calc triple]D'.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-ir -module-name M \
// RUN:   -import-objc-header %t/Calc.h \
// RUN:   -enable-experimental-feature ObjCDirect \
// RUN:   -enable-experimental-feature ObjCImplementation \
// RUN:   -target %target-stable-abi-triple %t/impl.swift -o %t/out.ll
// RUN: %FileCheck %s < %t/out.ll
// RUN: %FileCheck %s --check-prefix=ABSENT < %t/out.ll

// RUN: %target-swift-frontend -emit-silgen -module-name M \
// RUN:   -import-objc-header %t/Calc.h \
// RUN:   -enable-experimental-feature ObjCDirect \
// RUN:   -enable-experimental-feature ObjCImplementation \
// RUN:   -target %target-stable-abi-triple \
// RUN:   -primary-file %t/header.swift %t/impl.swift \
// RUN:   | %FileCheck %s --check-prefix=HEADER

// ABSENT-NOT: "\01-[Calc
// ABSENT-NOT: "\01+[Calc

//--- Calc.h

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

typedef struct { long a, b, c; } Triple;

@interface Calc : NSObject
- (instancetype)init NS_DESIGNATED_INITIALIZER;
- (instancetype)initWithBase:(long)base __attribute__((objc_direct));
- (Triple)triple __attribute__((objc_direct));
- (NSString *)name __attribute__((objc_direct));
+ (long)make __attribute__((objc_direct));
@property (direct) long count;
@end

@interface Calc (Extra)
- (long)extra __attribute__((objc_direct));
@end

NS_ASSUME_NONNULL_END

//--- impl.swift

import Foundation

@objc @implementation extension Calc {
  @objcDirect var count: Int = 0

  override init() { super.init() }
  @objcDirect convenience init(base: Int) {
    self.init()
    count = base
  }

  @objcDirect func triple() -> Triple { Triple(a: 1, b: 2, c: 3) }
  @objcDirect func name() -> String! { "calc" }
  @objcDirect class func make() -> Int { 42 }
}

@objc(Extra) @implementation extension Calc {
  @objcDirect func extra() -> Int { 7 }
}

// CHECK-LABEL: define {{.*}}@"$s1M3useyyF"()
// CHECK: call {{.*}}@"$sSo4CalcC1ME4baseABSi_tcfC"(
// CHECK: call {{.*}}@"$sSo4CalcC1ME6tripleSo6TripleayF"(
// CHECK: call {{.*}}@"$sSo4CalcC1ME5countSivs"(
// CHECK: call {{.*}}@"$sSo4CalcC1ME5countSivg"(
// CHECK: call {{.*}}@"$sSo4CalcC1ME4makeSiyFZ"(
// CHECK: call {{.*}}@"$sSo4CalcC1ME5extraSiyF"(
func use() {
  let c = Calc(base: 1)
  _ = c.triple()
  c.count = 2
  _ = c.count
  _ = Calc.make()
  _ = c.extra()
}

// So does a protocol witness, even 'modify', which has no selector.
// CHECK-LABEL: define {{.*}}@"$sSo4CalcC1M8CountingA2cDP5countSivMTW"(
// CHECK: call {{.*}}@"$sSo4CalcC1ME5countSivM"(
protocol Counting { var count: Int { get set } }
extension Calc: Counting {}

//--- header.swift

import Foundation

// These still call through the header. 'name()' returns 'String!' in Swift.
// HEADER-LABEL: sil hidden [ossa] @$s1M8mismatchySSSo4CalcCF :
// HEADER: function_ref @$sSo4CalcC4nameSSyFTo :
func mismatch(_ c: Calc) -> String { c.name() }

// Code that may be inlined into another module can't see the implementation.
// HEADER-LABEL: sil [serialized] [ossa] @$s1M7inlinedSiyF :
// HEADER: function_ref @$sSo4CalcC4makeSiyFZTo :
@inlinable public func inlined() -> Int { Calc.make() }
