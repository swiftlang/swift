// Please keep this file in alphabetical order!

// RUN: %empty-directory(%t)

// FIXME: BEGIN -enable-source-import hackaround
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift -disable-objc-attr-requires-foundation-module
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource) -I %t -emit-module -o %t %s -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource) -I %t -parse-as-library %t/protocols.swiftmodule -typecheck -emit-objc-header-path %t/protocols.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t/protocols.h
// RUN: %FileCheck --check-prefix=NEGATIVE %s < %t/protocols.h
// RUN: %check-in-clang %t/protocols.h

// REQUIRES: objc_interop

import Foundation
import objc_generics

// CHECK-LABEL: @protocol A{{$}}
// CHECK-NEXT: @end
@objc protocol A {}

// CHECK-LABEL: @protocol B <A>
// CHECK-NEXT: @end
@objc protocol B : A {}

// CHECK: @protocol CustomName2;
// CHECK-LABEL: SWIFT_PROTOCOL_NAMED("CustomName")
// CHECK-NEXT: @protocol CustomName{{$}}
// CHECK-NEXT: - (void)forwardCustomName:(id <CustomName2> _Nonnull)_;
// CHECK-NEXT: @end
@objc(CustomName)
protocol CustomName {
  func forwardCustomName(_: CustomNameType2)
}

// CHECK-LABEL: SWIFT_PROTOCOL_NAMED("CustomNameType2")
// CHECK-NEXT: @protocol CustomName2{{$}}
// CHECK-NEXT: @end
@objc(CustomName2)
protocol CustomNameType2 {}

// CHECK-LABEL: @protocol Initializers{{$}}
// CHECK-NEXT: - (nonnull instancetype)init;
// CHECK-NEXT: - (nonnull instancetype)initWithObject:(id _Nonnull)any;
// CHECK-NEXT: @end
@objc protocol Initializers {
  init()
  init(object any: AnyObject)
}

// CHECK-LABEL: @interface MyObject : NSObject <NSCoding, Fungible>
// CHECK-NEXT: initWithCoder
// CHECK-NEXT: init SWIFT_UNAVAILABLE
// CHECK-NEXT: new SWIFT_DEPRECATED
// CHECK-NEXT: @end
// NEGATIVE-NOT: @protocol NSCoding
class MyObject : NSObject, NSCoding, Fungible {
  required init(coder aCoder: NSCoder) {
    super.init()
  }
}

// CHECK-LABEL: @protocol Methods{{$}}
// CHECK-NEXT: - (void)test;
// CHECK-NEXT: + (void)test2;
// CHECK-NEXT: - (void)testRawAnyTypes:(id _Nonnull)any other:(Class _Nonnull)other;
// CHECK-NEXT: - (void)testSingleProtocolTypes:(id <A> _Nonnull)a aAgain:(id <A> _Nonnull)a2 b:(id <B> _Nonnull)b bAgain:(id <B> _Nonnull)b2 both:(id <B> _Nonnull)both;
// CHECK-NEXT: - (void)testSingleProtocolClassTypes:(Class <A> _Nonnull)a aAgain:(Class <A> _Nonnull)a2 b:(Class <B> _Nonnull)b bAgain:(Class <B> _Nonnull)b2 both:(Class <B> _Nonnull)both;
// CHECK-NEXT: - (void)testComposition:(id <A, ZZZ> _Nonnull)x meta:(Class <A, ZZZ> _Nonnull)xClass;
// CHECK-NEXT: - (void)testSubclassComposition:(MyObject <ZZZ> * _Nonnull)x meta:(SWIFT_METATYPE(MyObject) <ZZZ> _Nonnull)xClass;
// CHECK-NEXT: - (void)testGenericSubclassComposition:(FungibleContainer<MyObject *> <ZZZ> * _Nonnull)x meta:(SWIFT_METATYPE(FungibleContainer) <ZZZ> _Nonnull)xClass;
// CHECK-NEXT: - (void)testOptional:(id <A> _Nullable)opt meta:(Class <A> _Nullable)m;
// CHECK-NEXT: @end
@objc protocol Methods {
  func test()
  static func test2()

  func testRawAnyTypes(_ any: AnyObject, other: AnyObject.Type)

  func testSingleProtocolTypes(_ a : A, aAgain a2: A, b: B, bAgain b2: B, both: A & B)
  func testSingleProtocolClassTypes(_ a : A.Type, aAgain a2: A.Type, b: B.Type, bAgain b2: B.Type, both: (A & B).Type)
  func testComposition(_ x: A & ZZZ, meta xClass: (A & ZZZ).Type)
  func testSubclassComposition(_ x: MyObject & ZZZ, meta xClass: (MyObject & ZZZ).Type)
  func testGenericSubclassComposition(_ x: FungibleContainer<MyObject> & ZZZ, meta xClass: (FungibleContainer<MyObject> & ZZZ).Type)

  func testOptional(_ opt: A?, meta m: A.Type?)
}

// NEGATIVE-NOT: NotObjC
protocol NotObjC : class {}

// NEGATIVE-NOT: @interface NSString (SWIFT_EXTENSION(protocols)){{$}}
extension NSString : NotObjC {}

// CHECK-LABEL: @protocol ZZZ{{$}}
// CHECK-LABEL: @interface NSString (SWIFT_EXTENSION(protocols)) <A, ZZZ>
extension NSString : A, ZZZ {}

// CHECK-LABEL: @protocol Optional
// CHECK-NEXT: - (void)a;
// CHECK-NEXT: - (void)b;
// CHECK-NEXT: @optional
// CHECK-NEXT: - (void)c;
// CHECK-NEXT: - (void)d;
// CHECK-NEXT: @required
// CHECK-NEXT: - (void)e;
// CHECK-NEXT: @optional
// CHECK-NEXT: - (void)f;
// CHECK-NEXT: @end
@objc protocol Optional {
  func a()
  func b()

  @objc optional func c()
  @objc optional func d()

  func e()

  @objc optional func f()
}

// NEGATIVE-NOT: @protocol PrivateProto
@objc private protocol PrivateProto {}

// CHECK-LABEL: @interface PrivateProtoAdopter{{$}}
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc @objcMembers class PrivateProtoAdopter : PrivateProto {}

// CHECK-LABEL: @interface PrivateProtoAdopter2 <A>
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc @objcMembers class PrivateProtoAdopter2 : PrivateProto, A {}

// CHECK-LABEL: @protocol Properties
// CHECK-NEXT: @property (nonatomic, readonly) NSInteger a;
// CHECK-NEXT: @property (nonatomic, strong) id <Properties> _Nullable b;
// CHECK-NEXT: @optional
// CHECK-NEXT: @property (nonatomic, readonly, copy) NSString * _Nonnull c;
// CHECK-NEXT: @end
@objc protocol Properties {
  var a: Int { get }
  var b: Properties? { get set }
  @objc optional var c: String { get }
}


// Forward declaration of class referenced from subclass existential.

// CHECK-LABEL: @class ReferencesSomeClass2;

// CHECK-LABEL: @protocol ReferencesSomeClass1
// CHECK-NEXT: - (void)referencesWithSomeClassAndZZZ:(ReferencesSomeClass2 <ZZZ> * _Nonnull)someClassAndZZZ;
// CHECK-NEXT: @end

// CHECK-LABEL: @interface ReferencesSomeClass2
// CHECK-NEXT: - (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: @end

@objc protocol ReferencesSomeClass1 {
  @objc func references(someClassAndZZZ: ReferencesSomeClass2 & ZZZ)
}

@objc @objcMembers class ReferencesSomeClass2 {}


// CHECK-LABEL: @protocol ReversedOrder2{{$}}
// CHECK-NEXT: @end
// CHECK: SWIFT_PROTOCOL
// CHECK-NEXT: @protocol ReversedOrder1 <ReversedOrder2>
// CHECK-NEXT: @end
@objc protocol ReversedOrder1 : ReversedOrder2 {}
@objc protocol ReversedOrder2 {}


// CHECK-LABEL: @interface RootClass1{{$}}
// CHECK: @interface RootClass2 <A>{{$}}
// CHECK: @interface RootClass3 <B>{{$}}
@objc class RootClass1 : NotObjC {}
@objc class RootClass2 : A, NotObjC {}
@objc class RootClass3 : NotObjC, B {}

// CHECK: @interface RootClass4 <A, ZZZ>{{$}}
@objc class RootClass4 : A, ZZZ {}

// CHECK-LABEL: @interface Subclass : RootClass1 <ZZZ>{{$}}
@objc class Subclass : RootClass1, ZZZ {}

// CHECK-LABEL: @protocol UnownedProperty
// CHECK-NEXT: @property (nonatomic, unsafe_unretained) id _Nonnull unownedProp;
@objc protocol UnownedProperty {
  unowned var unownedProp: AnyObject { get set }
}

// CHECK-LABEL: @protocol WeakProperty
// CHECK-NEXT: @property (nonatomic, weak) id _Nullable weakProp;
@objc protocol WeakProperty {
  weak var weakProp: AnyObject? { get set }
}

// Deliberately at the end of the file.
@objc protocol ZZZ {}
