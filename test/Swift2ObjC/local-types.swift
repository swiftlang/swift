// Please keep this file in alphabetical order!

// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift %clang-importer-sdk -module-cache-path=%t/clang-module-cache -emit-module -o %t %s -module-name local
// RUN: %swift-ide-test %clang-importer-sdk -module-cache-path=%t/clang-module-cache -print-as-objc %t/local.swiftmodule -source-filename %s > %t/local.h
// RUN: FileCheck %s < %t/local.h

import ObjectiveC

// CHECK-LABEL: @interface AFullyDefinedClass
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
// CHECK-NEXT: @class ANonObjCClass;
// CHECK-NEXT: @interface UseForward
// CHECK-NEXT: - (void)definedAlready:(AFullyDefinedClass *)a;
// CHECK-NEXT: - (void)a:(ZForwardClass1 *)a;
// CHECK-NEXT: - (ZForwardClass2 *)b;
// CHECK-NEXT: - (void)c:(ZForwardAliasClass *)c;
// CHECK-NEXT: - (void)d:(id <ZForwardProtocol1>)d;
// CHECK-NEXT: - (void)e:(Class <ZForwardProtocol2>)e;
// CHECK-NEXT: - (void)e2:(id <ZForwardProtocol2>)e;
// CHECK-NEXT: - (void)f:(id <ZForwardProtocol5> (^)(id <ZForwardProtocol3>, id <ZForwardProtocol4>))f;
// CHECK-NEXT: - (void)g:(id <ZForwardProtocol6, ZForwardProtocol7>)g;
// CHECK-NEXT: - (void)h:(ANonObjCClass *)h;
// CHECK-NEXT: @end

@objc class UseForward {
  func definedAlready(a: AFullyDefinedClass) {}

  func a(a: ZForwardClass1) {}
  func b() -> ZForwardClass2 { return ZForwardClass2() }
  func c(c: ZForwardAlias) {}

  func d(d: (ZForwardProtocol1)) {}
  func e(e: ZForwardProtocol2.metatype) {}
  func e2(e: ZForwardProtocol2) {}
  func f(f: (ZForwardProtocol3, ZForwardProtocol4) -> ZForwardProtocol5) {}
  func g(g: protocol<ZForwardProtocol6, ZForwardProtocol7>) {}

  func h(h: ANonObjCClass) {}
}

// CHECK-NOT: @class ZForwardClass1;
// CHECK-NOT: @protocol ZForwardProtocol1;
// CHECK: @interface UseForwardAgain
// CHECK-NEXT: - (void)a:(ZForwardClass1 *)a;
// CHECK-NEXT: - (void)b:(id <ZForwardProtocol1>)b;
// CHECK-NEXT: @end
@objc class UseForwardAgain {
  func a(a: ZForwardClass1) {}
  func b(b: ZForwardProtocol1) {}
}

typealias ZForwardAlias = ZForwardAliasClass;
class ZForwardAliasClass {}

// CHECK-NOT: @class UseForward;
// CHECK: @interface ZForwardClass1
// CHECK-NEXT: - (void)circular:(UseForward *)a;
// CHECK-NEXT: @end
@objc class ZForwardClass1 {
  func circular(a: UseForward) {}
}
class ZForwardClass2 {}

@class_protocol @objc protocol ZForwardProtocol1 {}
@class_protocol @objc protocol ZForwardProtocol2 {}
@class_protocol @objc protocol ZForwardProtocol3 {}
@class_protocol @objc protocol ZForwardProtocol4 {}
@class_protocol @objc protocol ZForwardProtocol5 {}
@class_protocol @objc protocol ZForwardProtocol6 {}
@class_protocol @objc protocol ZForwardProtocol7 {}
