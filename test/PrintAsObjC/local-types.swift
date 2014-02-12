// Please keep this file in alphabetical order!

// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift %clang-importer-sdk -module-cache-path %t/clang-module-cache -emit-module -o %t %s -module-name local
// RUN: %swift-ide-test %clang-importer-sdk -module-cache-path %t/clang-module-cache -print-as-objc %t/local.swiftmodule -source-filename %s > %t/local.h
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
// CHECK-NEXT: - (void)definedAlready:(AFullyDefinedClass *)a;
// CHECK-NEXT: - (void)a:(ZForwardClass1 *)a;
// CHECK-NEXT: - (ZForwardClass2 *)b;
// CHECK-NEXT: - (void)c:(ZForwardAliasClass *)c;
// CHECK-NEXT: - (void)d:(id <ZForwardProtocol1>)d;
// CHECK-NEXT: - (void)e:(Class <ZForwardProtocol2>)e;
// CHECK-NEXT: - (void)e2:(id <ZForwardProtocol2>)e;
// CHECK-NEXT: - (void)f:(id <ZForwardProtocol5> (^)(id <ZForwardProtocol3>, id <ZForwardProtocol4>))f;
// CHECK-NEXT: - (void)g:(id <ZForwardProtocol6, ZForwardProtocol7>)g;
// CHECK-NEXT: - (Class)h:(id)h;
// CHECK-NEXT: - (void)i:(id <ZForwardProtocol8>)_;
// CHECK-NEXT: @property (nonatomic, readonly) ZForwardClass3 * j;
// CHECK-NEXT: @property (nonatomic, readonly) SWIFT_METATYPE(ZForwardClass4) k;
// CHECK-NEXT: init
// CHECK-NEXT: @end

@objc class UseForward {
  func definedAlready(a: AFullyDefinedClass) {}

  func a(a: ZForwardClass1) {}
  func b() -> ZForwardClass2 { return ZForwardClass2() }
  func c(c: ZForwardAlias) {}

  func d(d: (ZForwardProtocol1)) {}
  func e(e: ZForwardProtocol2.metatype) {}
  func e2(e: ZForwardProtocol2) {}
  func f(f: @objc_block (ZForwardProtocol3, ZForwardProtocol4) -> ZForwardProtocol5) {}
  func g(g: protocol<ZForwardProtocol6, ZForwardProtocol7>) {}

  func h(h: ANonObjCClass) -> ANonObjCClass.metatype { return typeof(h) }
  func i(_: ZForwardProtocol8) {}

  var j: ZForwardClass3 { return ZForwardClass3() }
  var k: ZForwardClass4.metatype { return ZForwardClass4 }
}

// CHECK-NOT: @class ZForwardClass1;
// CHECK-NOT: @protocol ZForwardProtocol1;

// CHECK-LABEL: @interface UseForwardAgain
// CHECK-NEXT: - (void)a:(ZForwardClass1 *)a;
// CHECK-NEXT: - (void)b:(id <ZForwardProtocol1>)b;
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
// CHECK-NEXT: - (void)circular:(UseForward *)a;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class ZForwardClass1 {
  func circular(a: UseForward) {}
}
@objc class ZForwardClass2 {}
@objc class ZForwardClass3 {}
@objc class ZForwardClass4 {}

@class_protocol @objc protocol ZForwardProtocol1 {}
@class_protocol @objc protocol ZForwardProtocol2 {}
@class_protocol @objc protocol ZForwardProtocol3 {}
@class_protocol @objc protocol ZForwardProtocol4 {}
@class_protocol @objc protocol ZForwardProtocol5 {}
@class_protocol @objc protocol ZForwardProtocol6 {}
@class_protocol @objc protocol ZForwardProtocol7 {}
@class_protocol @objc protocol ZForwardProtocol8 {}
