// Please keep this file in alphabetical order!

// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %s -module-name local -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %t/local.swiftmodule -typecheck -emit-objc-header-path %t/local.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t/local.h
// RUN: %check-in-clang %t/local.h

// REQUIRES: objc_interop

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
// CHECK-NEXT: - (void)definedAlready:(AFullyDefinedClass * _Nonnull)a;
// CHECK-NEXT: - (void)a:(ZForwardClass1 * _Nonnull)a;
// CHECK-NEXT: - (ZForwardClass2 * _Nonnull)b SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (void)c:(ZForwardAliasClass * _Nonnull)c;
// CHECK-NEXT: - (void)d:(id <ZForwardProtocol1> _Nonnull)d;
// CHECK-NEXT: - (void)e:(Class <ZForwardProtocol2> _Nonnull)e;
// CHECK-NEXT: - (void)e2:(id <ZForwardProtocol2> _Nonnull)e;
// CHECK-NEXT: - (void)f:(SWIFT_NOESCAPE id <ZForwardProtocol5> _Nonnull (^ _Nonnull)(id <ZForwardProtocol3> _Nonnull, id <ZForwardProtocol4> _Nonnull))f;
// CHECK-NEXT: - (void)g:(id <ZForwardProtocol6, ZForwardProtocol7> _Nonnull)g;
// CHECK-NEXT: - (void)i:(id <ZForwardProtocol8> _Nonnull)_;
// CHECK-NEXT: @property (nonatomic, readonly, strong) ZForwardClass3 * _Nonnull j;
// CHECK-NEXT: @property (nonatomic, readonly) SWIFT_METATYPE(ZForwardClass4) _Nonnull k;
// CHECK-NEXT: init
// CHECK-NEXT: @end

@objc class UseForward {
  @objc func definedAlready(_ a: AFullyDefinedClass) {}

  @objc func a(_ a: ZForwardClass1) {}
  @objc func b() -> ZForwardClass2 { return ZForwardClass2() }
  @objc func c(_ c: ZForwardAlias) {}

  @objc func d(_ d: (ZForwardProtocol1)) {}
  @objc func e(_ e: ZForwardProtocol2.Type) {}
  @objc func e2(_ e: ZForwardProtocol2) {}
  @objc func f(_ f: (ZForwardProtocol3, ZForwardProtocol4) -> ZForwardProtocol5) {}
  @objc func g(_ g: ZForwardProtocol6 & ZForwardProtocol7) {}

  func h(_ h: ANonObjCClass) -> ANonObjCClass.Type { return type(of: h) }
  @objc func i(_: ZForwardProtocol8) {}

  @objc var j: ZForwardClass3 { return ZForwardClass3() }
  @objc var k: ZForwardClass4.Type { return ZForwardClass4.self }
}

// CHECK-NOT: @class ZForwardClass1;
// CHECK-NOT: @protocol ZForwardProtocol1;

// CHECK-LABEL: @interface UseForwardAgain
// CHECK-NEXT: - (void)a:(ZForwardClass1 * _Nonnull)a;
// CHECK-NEXT: - (void)b:(id <ZForwardProtocol1> _Nonnull)b;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class UseForwardAgain {
  @objc func a(_ a: ZForwardClass1) {}
  @objc func b(_ b: ZForwardProtocol1) {}
}

typealias ZForwardAlias = ZForwardAliasClass
@objc class ZForwardAliasClass {}

// CHECK-NOT: @class UseForward;

// CHECK-LABEL: @interface ZForwardClass1
// CHECK-NEXT: - (void)circular:(UseForward * _Nonnull)a;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class ZForwardClass1 {
  @objc func circular(_ a: UseForward) {}
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
