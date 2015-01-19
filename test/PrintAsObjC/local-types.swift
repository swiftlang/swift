// Please keep this file in alphabetical order!

// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend %clang-importer-sdk -emit-module -o %t %s -module-name local -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend %clang-importer-sdk -parse-as-library %t/local.swiftmodule -parse -emit-objc-header-path %t/local.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: FileCheck %s < %t/local.h
// RUN: %check-in-clang %t/local.h

import ObjectiveC

// CHECK-LABEL: @interface AFullyDefinedClass
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class AFullyDefinedClass {}

class ANonObjCClass {}

// CHECK-NOT: @class AFullyDefinedClass
// CHECK: @class ZForwardClass1;
// CHECK-NEXT: @class ZForwardClass2;
// CHECK-NEXT: @class ZForwardAliasClass;
// CHECK-NEXT: @protocol ZForwardProtocol1;
// CHECK-NEXT: @protocol ZForwardProtocol2;
// CHECK-NEXT: @protocol ZForwardProtocol3;
// CHECK-NEXT: @protocol ZForwardProtocol4;
// CHECK-NEXT: @protocol ZForwardProtocol5;
// CHECK-NEXT: @protocol ZForwardProtocol6;
// CHECK-NEXT: @protocol ZForwardProtocol7;
// CHECK-NEXT: @protocol ZForwardProtocol8;
// CHECK-NEXT: @class ZForwardClass3;

// CHECK-LABEL: @interface UseForward
// CHECK-NEXT: - (void)definedAlready:(AFullyDefinedClass * __nonnull)a;
// CHECK-NEXT: - (void)a:(ZForwardClass1 * __nonnull)a;
// CHECK-NEXT: - (ZForwardClass2 * __nonnull)b;
// CHECK-NEXT: - (void)c:(ZForwardAliasClass * __nonnull)c;
// CHECK-NEXT: - (void)d:(id <ZForwardProtocol1> __nonnull)d;
// CHECK-NEXT: - (void)e:(Class <ZForwardProtocol2> __nonnull)e;
// CHECK-NEXT: - (void)e2:(id <ZForwardProtocol2> __nonnull)e;
// CHECK-NEXT: - (void)f:(id <ZForwardProtocol5> __nonnull (^ __nonnull)(id <ZForwardProtocol3> __nonnull, id <ZForwardProtocol4> __nonnull))f;
// CHECK-NEXT: - (void)g:(id <ZForwardProtocol6, ZForwardProtocol7> __nonnull)g;
// CHECK-NEXT: - (void)i:(id <ZForwardProtocol8> __nonnull)_;
// CHECK-NEXT: @property (nonatomic, readonly) ZForwardClass3 * __nonnull j;
// CHECK-NEXT: @property (nonatomic, readonly) SWIFT_METATYPE(ZForwardClass4) __nonnull k;
// CHECK-NEXT: init
// CHECK-NEXT: @end

@objc class UseForward {
  func definedAlready(a: AFullyDefinedClass) {}

  func a(a: ZForwardClass1) {}
  func b() -> ZForwardClass2 { return ZForwardClass2() }
  func c(c: ZForwardAlias) {}

  func d(d: (ZForwardProtocol1)) {}
  func e(e: ZForwardProtocol2.Type) {}
  func e2(e: ZForwardProtocol2) {}
  func f(f: (ZForwardProtocol3, ZForwardProtocol4) -> ZForwardProtocol5) {}
  func g(g: protocol<ZForwardProtocol6, ZForwardProtocol7>) {}

  func h(h: ANonObjCClass) -> ANonObjCClass.Type { return h.dynamicType }
  func i(_: ZForwardProtocol8) {}

  var j: ZForwardClass3 { return ZForwardClass3() }
  var k: ZForwardClass4.Type { return ZForwardClass4.self }
}

// CHECK-NOT: @class ZForwardClass1;
// CHECK-NOT: @protocol ZForwardProtocol1;

// CHECK-LABEL: @interface UseForwardAgain
// CHECK-NEXT: - (void)a:(ZForwardClass1 * __nonnull)a;
// CHECK-NEXT: - (void)b:(id <ZForwardProtocol1> __nonnull)b;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class UseForwardAgain {
  func a(a: ZForwardClass1) {}
  func b(b: ZForwardProtocol1) {}
}

typealias ZForwardAlias = ZForwardAliasClass;
@objc class ZForwardAliasClass {}

// CHECK-NOT: @class UseForward;

// CHECK-LABEL: @interface ZForwardClass1
// CHECK-NEXT: - (void)circular:(UseForward * __nonnull)a;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class ZForwardClass1 {
  func circular(a: UseForward) {}
}
@objc class ZForwardClass2 {}
@objc class ZForwardClass3 {}
@objc class ZForwardClass4 {}

@objc protocol ZForwardProtocol1 {}
@objc protocol ZForwardProtocol2 {}
@objc protocol ZForwardProtocol3 {}
@objc protocol ZForwardProtocol4 {}
@objc protocol ZForwardProtocol5 {}
@objc protocol ZForwardProtocol6 {}
@objc protocol ZForwardProtocol7 {}
@objc protocol ZForwardProtocol8 {}
