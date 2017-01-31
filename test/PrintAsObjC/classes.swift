// Please keep this file in alphabetical order!

// REQUIRES: objc_interop

// RUN: rm -rf %t
// RUN: mkdir -p %t

// FIXME: BEGIN -enable-source-import hackaround
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/AppKit.swift
// FIXME: END -enable-source-import hackaround


// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -I %S/Inputs/custom-modules -o %t %s -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -parse-as-library %t/classes.swiftmodule -typecheck -I %S/Inputs/custom-modules -emit-objc-header-path %t/classes.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t/classes.h
// RUN: %FileCheck --check-prefix=NEGATIVE %s < %t/classes.h
// RUN: %check-in-clang -I %S/Inputs/custom-modules/ %t/classes.h
// RUN: not %check-in-clang -I %S/Inputs/custom-modules/ -fno-modules -Qunused-arguments %t/classes.h
// RUN: %check-in-clang -I %S/Inputs/custom-modules/ -fno-modules -Qunused-arguments %t/classes.h -include Foundation.h -include CoreFoundation.h -include objc_generics.h -include SingleGenericClass.h

// CHECK-NOT: AppKit;
// CHECK-NOT: Properties;
// CHECK-NOT: Swift;
// CHECK-LABEL: @import Foundation;
// CHECK-NEXT: @import CoreGraphics;
// CHECK-NEXT: @import CoreFoundation;
// CHECK-NEXT: @import objc_generics;
// CHECK-NEXT: @import SingleGenericClass;
// CHECK-NOT: AppKit;
// CHECK-NOT: Swift;
import Foundation
import objc_generics
import AppKit // only used in implementations
import CoreFoundation
import SingleGenericClass

// CHECK-LABEL: @interface A1{{$}}
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class A1 {}

// CHECK-LABEL: @interface B1 : A1
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class B1 : A1 {}

// CHECK-LABEL: @interface BridgedTypes
// CHECK-NEXT: - (NSDictionary * _Nonnull)dictBridge:(NSDictionary * _Nonnull)x SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (NSSet * _Nonnull)setBridge:(NSSet * _Nonnull)x SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class BridgedTypes {
  func dictBridge(_ x: Dictionary<NSObject, AnyObject>) -> Dictionary<NSObject, AnyObject> {
    return x
  }

  func setBridge(_ x: Set<NSObject>) -> Set<NSObject> {
    return x
  }
}

// CHECK: @class CustomName2;
// CHECK-LABEL: SWIFT_CLASS_NAMED("ClassWithCustomName")
// CHECK-NEXT: @interface CustomName{{$}}
// CHECK-NEXT: - (void)forwardCustomName:(CustomName2 * _Nonnull)_;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc(CustomName)
class ClassWithCustomName {
  func forwardCustomName(_: ClassWithCustomName2) {}
}
  
// CHECK-LABEL: SWIFT_CLASS_NAMED("ClassWithCustomName2")
// CHECK-NEXT: @interface CustomName2{{$}}
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc(CustomName2)
class ClassWithCustomName2 {}
  
// CHECK-LABEL: SWIFT_CLASS_NAMED("ClassWithCustomNameSub")
// CHECK-NEXT: @interface CustomNameSub : CustomName{{$}}
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc(CustomNameSub)
class ClassWithCustomNameSub : ClassWithCustomName {}


// CHECK-LABEL: @interface ClassWithNSObjectProtocol <NSObject>
// CHECK-NEXT: @property (nonatomic, readonly, copy) NSString * _Nonnull description;
// CHECK-NEXT: - (BOOL)conformsToProtocol:(Protocol * _Nonnull)_ SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (BOOL)isKindOfClass:(Class _Nonnull)aClass SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: @end
@objc class ClassWithNSObjectProtocol : NSObjectProtocol {
  var description: String { return "me" }
  @objc(conformsToProtocol:)
  func conforms(to _: Protocol) -> Bool { return false }

  @objc(isKindOfClass:)
  func isKind(of aClass: AnyClass) -> Bool { return false }
}

// CHECK-LABEL: @interface DiscardableResult : NSObject
// CHECK-NEXT: - (NSInteger)nonDiscardable:(NSInteger)x SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (NSInteger)discardable:(NSInteger)x;
// CHECK-NEXT: - (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: @end
class DiscardableResult : NSObject {
  func nonDiscardable(_ x: Int) -> Int { return x }
  @discardableResult func discardable(_ x: Int) -> Int { return x }
}

// CHECK-LABEL: @interface Initializers
// CHECK-NEXT: - (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: - (nonnull instancetype)initWithInt:(NSInteger)_;
// CHECK-NEXT: - (nonnull instancetype)initWithFloat:(float)f;
// CHECK-NEXT: - (nonnull instancetype)initWithString:(NSString * _Nonnull)s boolean:(BOOL)b;
// CHECK-NEXT: - (nullable instancetype)initWithBoolean:(BOOL)b;
// CHECK-NEXT: - (nonnull instancetype)foo_initWithInt:(NSInteger)_ SWIFT_METHOD_FAMILY(init);
// CHECK-NEXT: - (nonnull instancetype)initializeWithX:(NSInteger)_ SWIFT_METHOD_FAMILY(init);
// CHECK-NEXT: - (nonnull instancetype)initForFun OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: - (nonnull instancetype)initWithMoreFun OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: - (nonnull instancetype)initWithEvenMoreFun OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: @end
@objc class Initializers {
  init() {}

  convenience init(int _: Int) { self.init() }

  convenience init(float f: Float) { self.init() }
  convenience init(string s: String, boolean b: ObjCBool) { self.init() }

  convenience init?(boolean b: ObjCBool) { self.init() }

  @objc(foo_initWithInt:) convenience init(foo_int _: Int) { self.init() }
  @objc(initializeWithX:) convenience init(X _: Int) { self.init() }

  init(forFun: ()) { }

  init(moreFun: ()) { }

  init(evenMoreFun: ()) { }
}

// CHECK-LABEL: @interface InheritedInitializers
// CHECK-NEXT: - (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: - (nonnull instancetype)initWithFloat:(float)f SWIFT_UNAVAILABLE;
// CHECK-NEXT: - (nonnull instancetype)initWithMoreFun SWIFT_UNAVAILABLE;
// CHECK-NEXT: - (nonnull instancetype)initForFun SWIFT_UNAVAILABLE;
// CHECK-NEXT: - (nonnull instancetype)initWithEvenMoreFun SWIFT_UNAVAILABLE;
// CHECK-NEXT: @end
@objc class InheritedInitializers : Initializers {
  override init() {
    super.init()
  }

  private convenience init(float f: Float) { self.init() }

  private override init(moreFun: ()) { super.init() }
}

// CHECK-LABEL: @interface InheritedInitializersAgain
// CHECK-NEXT: - (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: - (nonnull instancetype)initWithEvenMoreFun OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: @end
@objc class InheritedInitializersAgain : InheritedInitializers {
  override init() {
    super.init()
  }

  init(evenMoreFun: ()) { super.init() }
}

// NEGATIVE-NOT: NotObjC
class NotObjC {}

// CHECK-LABEL: @interface Methods{{$}}
// CHECK-NEXT: - (void)test;
// CHECK-NEXT: + (void)test2;
// CHECK-NEXT: - (void * _Nonnull)testPrimitives:(BOOL)b i:(NSInteger)i f:(float)f d:(double)d u:(NSUInteger)u SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (void)testString:(NSString * _Nonnull)s;
// CHECK-NEXT: - (void)testSelector:(SEL _Nonnull)sel boolean:(BOOL)b;
// CHECK-NEXT: - (void)testCSignedTypes:(signed char)a b:(short)b c:(int)c d:(long)d e:(long long)e;
// CHECK-NEXT: - (void)testCUnsignedTypes:(unsigned char)a b:(unsigned short)b c:(unsigned int)c d:(unsigned long)d e:(unsigned long long)e;
// CHECK-NEXT: - (void)testCChars:(char)basic wchar:(wchar_t)wide char16:(char16_t)char16 char32:(char32_t)char32;
// CHECK-NEXT: - (void)testCFloats:(float)a b:(double)b;
// CHECK-NEXT: - (void)testCBool:(bool)a;
// CHECK-NEXT: - (void)testSizedSignedTypes:(int8_t)a b:(int16_t)b c:(int32_t)c d:(int64_t)d;
// CHECK-NEXT: - (void)testSizedUnsignedTypes:(uint8_t)a b:(uint16_t)b c:(uint32_t)c d:(uint64_t)d;
// CHECK-NEXT: - (void)testSizedFloats:(float)a b:(double)b;
// CHECK-NEXT: - (nonnull instancetype)getDynamicSelf SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: + (SWIFT_METATYPE(Methods) _Nonnull)getSelf SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (Methods * _Nullable)maybeGetSelf SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: + (SWIFT_METATYPE(Methods) _Nullable)maybeGetSelf SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (Methods * _Null_unspecified)uncheckedGetSelf SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: + (SWIFT_METATYPE(Methods) _Null_unspecified)uncheckedGetSelf SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: + (SWIFT_METATYPE(CustomName) _Nonnull)getCustomNameType SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (void)testParens:(NSInteger)a;
// CHECK-NEXT: - (void)testIgnoredParam:(NSInteger)_;
// CHECK-NEXT: - (void)testIgnoredParams:(NSInteger)_ again:(NSInteger)_;
// CHECK-NEXT: - (void)testArrayBridging:(NSArray<Methods *> * _Nonnull)a;
// CHECK-NEXT: - (void)testArrayBridging2:(NSArray * _Nonnull)a;
// CHECK-NEXT: - (void)testArrayBridging3:(NSArray<NSString *> * _Nonnull)a;
// CHECK-NEXT: - (void)testDictionaryBridging:(NSDictionary * _Nonnull)a;
// CHECK-NEXT: - (void)testDictionaryBridging2:(NSDictionary<NSNumber *, Methods *> * _Nonnull)a;
// CHECK-NEXT: - (void)testDictionaryBridging3:(NSDictionary<NSString *, NSString *> * _Nonnull)a;
// CHECK-NEXT: - (void)testSetBridging:(NSSet * _Nonnull)a;
// CHECK-NEXT: - (IBAction)actionMethod:(id _Nonnull)_;
// CHECK-NEXT: - (void)methodWithReservedParameterNames:(id _Nonnull)long_ protected:(id _Nonnull)protected_;
// CHECK-NEXT: - (void)honorRenames:(CustomName * _Nonnull)_;
// CHECK-NEXT: - (Methods * _Nullable __unsafe_unretained)unmanaged:(id _Nonnull __unsafe_unretained)_ SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (void)initAllTheThings SWIFT_METHOD_FAMILY(none);
// CHECK-NEXT: - (void)initTheOtherThings SWIFT_METHOD_FAMILY(none);
// CHECK-NEXT: - (void)initializeEvenMoreThings;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class Methods {
  func test() {}
  class func test2() {}

  func testPrimitives(_ b: Bool, i: Int, f: Float, d: Double, u: UInt)
    -> OpaquePointer { return OpaquePointer(bitPattern: -1)! }
  func testString(_ s: String) {}
  func testSelector(_ sel: Selector, boolean b: ObjCBool) {}

  func testCSignedTypes(_ a: CSignedChar, b: CShort, c: CInt, d: CLong, e: CLongLong) {}
  func testCUnsignedTypes(_ a: CUnsignedChar, b: CUnsignedShort, c: CUnsignedInt, d: CUnsignedLong, e: CUnsignedLongLong) {}
  func testCChars(_ basic: CChar, wchar wide: CWideChar, char16: CChar16, char32: CChar32) {}
  func testCFloats(_ a: CFloat, b: CDouble) {}
  func testCBool(_ a: CBool) {}

  func testSizedSignedTypes(_ a: Int8, b: Int16, c: Int32, d: Int64) {}
  func testSizedUnsignedTypes(_ a: UInt8, b: UInt16, c: UInt32, d: UInt64) {}
  func testSizedFloats(_ a: Float32, b: Float64) {}

  func getDynamicSelf() -> Self { return self }
  class func getSelf() -> Methods.Type { return self }

  func maybeGetSelf() -> Methods? { return nil }
  class func maybeGetSelf() -> Methods.Type? { return self }
  func uncheckedGetSelf() -> Methods! { return self }
  class func uncheckedGetSelf() -> Methods.Type! { return self }

  class func getCustomNameType() -> ClassWithCustomName.Type {
    return ClassWithCustomName.self
  }

  func testParens(_ a: ((Int))) {}

  func testIgnoredParam(_: Int) {}
  func testIgnoredParams(_: Int, again _: Int) {}

  func testArrayBridging(_ a: [Methods]) {}
  func testArrayBridging2(_ a: [AnyObject]) {}
  func testArrayBridging3(_ a: [String]) {}

  func testDictionaryBridging(_ a: [NSObject : AnyObject]) {}
  func testDictionaryBridging2(_ a: [NSNumber : Methods]) {}
  func testDictionaryBridging3(_ a: [String : String]) {}

  func testSetBridging(_ a: Set<NSObject>) {}

  @IBAction func actionMethod(_: AnyObject) {}

  func methodWithReservedParameterNames(_ long: AnyObject, protected: AnyObject) {}

  func honorRenames(_: ClassWithCustomName) {}

  func unmanaged(_: Unmanaged<AnyObject>) -> Unmanaged<Methods>? { return nil }

  func initAllTheThings() {}
  @objc(initTheOtherThings) func setUpOtherThings() {}
  func initializeEvenMoreThings() {}
}

typealias AliasForNSRect = NSRect

// CHECK-LABEL: @class NSURL;
// NEGATIVE-NOT: @class CFTree
// CHECK-LABEL: @interface MethodsWithImports
// CHECK-NEXT: - (NSPoint)getOrigin:(NSRect)r SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (CGFloat)getOriginX:(NSRect)r SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (CGFloat)getOriginY:(CGRect)r SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (NSArray * _Nonnull)emptyArray SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (NSArray * _Nullable)maybeArray SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (NSRuncingMode)someEnum SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (Class <NSCoding> _Nullable)protocolClass SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (struct _NSZone * _Nullable)zone SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (CFTypeRef _Nullable)cf:(CFTreeRef _Nonnull)x str:(CFStringRef _Nonnull)str str2:(CFMutableStringRef _Nonnull)str2 obj:(CFAliasForTypeRef _Nonnull)obj SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (void)appKitInImplementation;
// CHECK-NEXT: - (NSURL * _Nullable)returnsURL SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class MethodsWithImports {
  func getOrigin(_ r: NSRect) -> NSPoint { return r.origin }
  func getOriginX(_ r: AliasForNSRect) -> CGFloat { return r.origin.x }
  func getOriginY(_ r: CGRect) -> CGFloat { return r.origin.y }

  func emptyArray() -> NSArray { return NSArray() }
  func maybeArray() -> NSArray? { return nil }

  func someEnum() -> NSRuncingMode { return .mince }
  func protocolClass() -> NSCoding.Type? { return nil }

  func zone() -> NSZone? { return nil }

  func cf(_ x: CFTree, str: CFString, str2: CFMutableString, obj: CFAliasForType) -> CFTypeRef? { return nil }

  func appKitInImplementation() {
    let _ : NSResponder?
  }

  func returnsURL() -> NSURL? { return nil }
}

// CHECK-LABEL: @interface MethodsWithPointers
// CHECK-NEXT: - (id _Nonnull * _Nonnull)test:(NSInteger * _Nonnull)a SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (void)testNested:(NSInteger * _Nonnull * _Nonnull)a;
// CHECK-NEXT: - (void)testBridging:(NSInteger const * _Nonnull)a b:(NSInteger * _Nonnull)b c:(Methods * _Nonnull * _Nonnull)c;
// CHECK-NEXT: - (void)testBridgingVoid:(void * _Nonnull)a b:(void const * _Nonnull)b;
// CHECK-NEXT: - (void)testBridgingOptionality:(NSInteger const * _Nullable)a b:(NSInteger * _Null_unspecified)b c:(Methods * _Nullable * _Nullable)c;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class MethodsWithPointers {
  func test(_ a: UnsafeMutablePointer<Int>) -> UnsafeMutablePointer<AnyObject> {
    return UnsafeMutablePointer(bitPattern: -1)!
  }

  func testNested(_ a: UnsafeMutablePointer<UnsafeMutablePointer<Int>>) {}

  func testBridging(_ a: UnsafePointer<Int>, b: UnsafeMutablePointer<Int>, c: AutoreleasingUnsafeMutablePointer<Methods>) {}
  func testBridgingVoid(_ a: UnsafeMutableRawPointer, b: UnsafeRawPointer) {}

  func testBridgingOptionality(_ a: UnsafePointer<Int>?, b: UnsafeMutablePointer<Int>!, c: AutoreleasingUnsafeMutablePointer<Methods?>?) {}
}

// CHECK-LABEL: @interface MyObject : NSObject
// CHECK-NEXT: init
// CHECK-NEXT: @end
// NEGATIVE-NOT: @interface NSObject
class MyObject : NSObject {}

// CHECK-LABEL: @protocol MyProtocol <NSObject>
// CHECK-NEXT: @end
@objc protocol MyProtocol : NSObjectProtocol {}

// CHECK-LABEL: @protocol MyProtocolMetaOnly;
// CHECK-LABEL: @interface MyProtocolMetaCheck
// CHECK-NEXT: - (void)test:(Class <MyProtocolMetaOnly> _Nullable)x;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class MyProtocolMetaCheck {
  func test(_ x: MyProtocolMetaOnly.Type?) {}
}
// CHECK-LABEL: @protocol MyProtocolMetaOnly
// CHECK-NEXT: @end
@objc protocol MyProtocolMetaOnly {}

// CHECK-LABEL: @interface Nested
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class Nested {
  // CHECK-LABEL: @interface Inner
  // CHECK-NEXT: init
  // CHECK-NEXT: @end
  @objc class Inner {
    // CHECK-LABEL: @interface DeeperIn
    // CHECK-NEXT: init
    // CHECK-NEXT: @end
    @objc class DeeperIn {}
  }

  // CHECK-LABEL: @interface AnotherInner : A1
  // CHECK-NEXT: init
  // CHECK-NEXT: @end
  @objc class AnotherInner : A1 {}

  // NEGATIVE-NOT: NonObjCInner
  class NonObjCInner {}

  // NEGATIVE-NOT: ImplicitObjCInner
  class ImplicitObjCInner : A1 {}
}

// CHECK-LABEL: @class Inner2;
// CHECK-LABEL: @interface NestedMembers
// CHECK-NEXT: @property (nonatomic, strong) Inner2 * _Nullable ref2;
// CHECK-NEXT: @property (nonatomic, strong) Inner3 * _Nullable ref3;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class NestedMembers {
  // NEGATIVE-NOT: @class NestedMembers;
  // CHECK-LABEL: @interface Inner2
  // CHECK-NEXT: @property (nonatomic, strong) NestedMembers * _Nullable ref;
  // CHECK-NEXT: init
  // CHECK-NEXT: @end
  @objc class Inner2 {
    var ref: NestedMembers?
  }

  var ref2: Inner2?
  var ref3: Inner3?

  // CHECK-LABEL: @interface Inner3
  // CHECK-NEXT: @property (nonatomic, strong) NestedMembers * _Nullable ref;
  // CHECK-NEXT: init
  // CHECK-NEXT: @end
  @objc class Inner3 {
    var ref: NestedMembers?
  }
}

// CHECK-LABEL: @interface NestedSuperclass
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class NestedSuperclass {
  // CHECK-LABEL: @interface Subclass : NestedSuperclass
  // CHECK-NEXT: init
  // CHECK-NEXT: @end
  @objc class Subclass : NestedSuperclass {}
}

// NEGATIVE-NOT: @interface Private :
private class Private : A1 {}

// CHECK-LABEL: @interface PrivateMembers
// CHECK-NEXT: @end
@objc class PrivateMembers {
  private var i = 0
  private func foo() {}
  private init() {}
  @objc private func bar() {}
}

public class NonObjCClass { }

// CHECK-LABEL: @interface Properties
// CHECK-NEXT: @property (nonatomic) NSInteger i;
// CHECK-NEXT: @property (nonatomic, readonly, strong) Properties * _Nonnull mySelf;
// CHECK-NEXT: @property (nonatomic, readonly) double pi;
// CHECK-NEXT: @property (nonatomic) NSInteger computed;
// CHECK-NEXT: SWIFT_CLASS_PROPERTY(@property (nonatomic, class, strong) Properties * _Nonnull shared;)
// CHECK-NEXT: + (Properties * _Nonnull)shared SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: + (void)setShared:(Properties * _Nonnull)newValue;
// CHECK-NEXT: SWIFT_CLASS_PROPERTY(@property (nonatomic, class, readonly, strong) Properties * _Nonnull sharedRO;)
// CHECK-NEXT: + (Properties * _Nonnull)sharedRO SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: @property (nonatomic, weak) Properties * _Nullable weakOther;
// CHECK-NEXT: @property (nonatomic, assign) Properties * _Nonnull unownedOther;
// CHECK-NEXT: @property (nonatomic, unsafe_unretained) Properties * _Nonnull unmanagedOther;
// CHECK-NEXT: @property (nonatomic, unsafe_unretained) Properties * _Nullable unmanagedByDecl;
// CHECK-NEXT: @property (nonatomic, weak) id <MyProtocol> _Nullable weakProto;
// CHECK-NEXT: @property (nonatomic) CFTypeRef _Nullable weakCF;
// CHECK-NEXT: @property (nonatomic) CFStringRef _Nullable weakCFString;
// CHECK-NEXT: @property (nonatomic) CFTypeRef _Nullable strongCF;
// CHECK-NEXT: @property (nonatomic) CFTypeRef _Nullable strongCFAlias;
// CHECK-NEXT: @property (nonatomic) CFAliasForTypeRef _Nullable anyCF;
// CHECK-NEXT: @property (nonatomic) CFAliasForTypeRef _Nullable anyCF2;
// CHECK-NEXT: @property (nonatomic, weak) IBOutlet id _Null_unspecified outlet;
// CHECK-NEXT: @property (nonatomic, strong) IBOutlet Properties * _Null_unspecified typedOutlet;
// CHECK-NEXT: @property (nonatomic, copy) NSString * _Nonnull string;
// CHECK-NEXT: @property (nonatomic, copy) NSArray * _Nonnull array;
// CHECK-NEXT: @property (nonatomic, copy) NSArray<NSArray<NSNumber *> *> * _Nonnull arrayOfArrays;
// CHECK-NEXT: @property (nonatomic, copy) NSArray<BOOL (^)(id _Nonnull, NSInteger)> * _Nonnull arrayOfBlocks;
// CHECK-NEXT: @property (nonatomic, copy) NSArray<NSArray<void (^)(void)> *> * _Nonnull arrayOfArrayOfBlocks;
// CHECK-NEXT: @property (nonatomic, copy) NSDictionary<NSString *, NSString *> * _Nonnull dictionary;
// CHECK-NEXT: @property (nonatomic, copy) NSDictionary<NSString *, NSNumber *> * _Nonnull dictStringInt;
// CHECK-NEXT: @property (nonatomic, copy) NSSet<NSString *> * _Nonnull stringSet;
// CHECK-NEXT: @property (nonatomic, copy) NSSet<NSNumber *> * _Nonnull intSet;
// CHECK-NEXT: @property (nonatomic, copy) NSArray<NSNumber *> * _Nonnull cgFloatArray;
// CHECK-NEXT: @property (nonatomic, copy) NSArray<NSValue *> * _Nonnull rangeArray;
// CHECK-NEXT: @property (nonatomic, copy) IBOutletCollection(Properties) NSArray<Properties *> * _Null_unspecified outletCollection;
// CHECK-NEXT: @property (nonatomic, copy) IBOutletCollection(CustomName) NSArray<CustomName *> *  _Nullable outletCollectionOptional;
// CHECK-NEXT: @property (nonatomic, copy) IBOutletCollection(id) NSArray * _Nullable outletCollectionAnyObject;
// CHECK-NEXT: @property (nonatomic, copy) IBOutletCollection(id) NSArray<id <NSObject>> * _Nullable outletCollectionProto;
// CHECK-NEXT: SWIFT_CLASS_PROPERTY(@property (nonatomic, class, readonly) NSInteger staticInt;)
// CHECK-NEXT: + (NSInteger)staticInt SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: SWIFT_CLASS_PROPERTY(@property (nonatomic, class, copy) NSString * _Nonnull staticString;)
// CHECK-NEXT: + (NSString * _Nonnull)staticString SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: + (void)setStaticString:(NSString * _Nonnull)value;
// CHECK-NEXT: SWIFT_CLASS_PROPERTY(@property (nonatomic, class, readonly) double staticDouble;)
// CHECK-NEXT: + (double)staticDouble SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: SWIFT_CLASS_PROPERTY(@property (nonatomic, class, readonly, copy) NSDictionary<NSString *, NSString *> * _Nonnull staticDictionary;)
// CHECK-NEXT: + (NSDictionary<NSString *, NSString *> * _Nonnull)staticDictionary SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: @property (nonatomic, strong) Properties * _Nullable wobble;
// CHECK-NEXT: @property (nonatomic, getter=isEnabled, setter=setIsEnabled:) BOOL enabled;
// CHECK-NEXT: @property (nonatomic) BOOL isAnimated;
// CHECK-NEXT: @property (nonatomic, getter=register, setter=setRegister:) BOOL register_;
// CHECK-NEXT: @property (nonatomic, readonly, strong, getter=this) Properties * _Nonnull this_;
// CHECK-NEXT: @property (nonatomic, readonly) NSInteger privateSetter;
// CHECK-NEXT: @property (nonatomic, readonly, getter=customGetterNameForPrivateSetter) BOOL privateSetterCustomNames;
// CHECK-NEXT: SWIFT_CLASS_PROPERTY(@property (nonatomic, class, readonly) NSInteger privateSetter;)
// CHECK-NEXT: + (NSInteger)privateSetter SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: SWIFT_CLASS_PROPERTY(@property (nonatomic, class, readonly, getter=customGetterNameForPrivateSetter) BOOL privateSetterCustomNames;)
// CHECK-NEXT: + (BOOL)customGetterNameForPrivateSetter SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: SWIFT_CLASS_PROPERTY(@property (nonatomic, class, readonly) NSInteger sharedConstant;)
// CHECK-NEXT: + (NSInteger)sharedConstant SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: @property (nonatomic) NSInteger initContext;
// CHECK-NEXT: - (NSInteger)initContext SWIFT_METHOD_FAMILY(none) SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: @property (nonatomic, readonly) NSInteger initContextRO;
// CHECK-NEXT: - (NSInteger)initContextRO SWIFT_METHOD_FAMILY(none) SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: @property (nonatomic, getter=initGetter) BOOL getterIsInit;
// CHECK-NEXT: - (BOOL)initGetter SWIFT_METHOD_FAMILY(none) SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: @property (nonatomic, setter=initSetter:) BOOL setterIsInit;
// CHECK-NEXT: - (void)initSetter:(BOOL)newValue SWIFT_METHOD_FAMILY(none);
// CHECK-NEXT: @property (nonatomic, copy) NSURL * _Nullable customValueTypeProp;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class Properties {
  var i: Int = 1
  var mySelf: Properties {
    return self
  }
  let pi = 3.14
  var computed: Int {
    get {
      return 42
    }
    set {
      // Ignore it.
    }
  }

  class var shared: Properties {
    get { return Properties() }
    set { }
  }

  class var sharedRO: Properties {
    get { return Properties() }
  }

  weak var weakOther: Properties?
  unowned var unownedOther: Properties = .shared
  unowned(unsafe) var unmanagedOther: Properties = .shared
  var unmanagedByDecl: Unmanaged<Properties>?

  weak var weakProto: MyProtocol?
  weak var weakCF: CFTypeRef?
  weak var weakCFString: CFString?

  typealias CFTypeRefAlias = CFTypeRef

  var strongCF: CFTypeRef?
  var strongCFAlias: CFTypeRefAlias?

  var anyCF: CFAliasForType?
  var anyCF2: CFAliasForType?

  @IBOutlet weak var outlet: AnyObject!
  @IBOutlet var typedOutlet: Properties!

  var string = "abc"
  var array: Array<AnyObject> = []
  var arrayOfArrays: Array<Array<Int>> = []
  var arrayOfBlocks: Array<@convention(block) (AnyObject, Int) -> Bool> = []
  var arrayOfArrayOfBlocks: Array<Array<@convention(block) () -> Void>> = []
  var dictionary: Dictionary<String, String> = [:]
  var dictStringInt: Dictionary<String, Int> = [:]
  var stringSet: Set<String> = []
  var intSet: Set<Int> = []
  var cgFloatArray: Array<CGFloat> = []
  var rangeArray: Array<NSRange> = []

  @IBOutlet var outletCollection: [Properties]!
  @IBOutlet var outletCollectionOptional: [ClassWithCustomName]? = []
  @IBOutlet var outletCollectionAnyObject: [AnyObject]?
  @IBOutlet var outletCollectionProto: [NSObjectProtocol]?

  static let staticInt = 2
  static var staticString = "Hello"
  static var staticDouble: Double {
    return 2.0
  }
  static var staticDictionary: [String: String] { return [:] }

  @objc(wobble) var wibble: Properties?

  var enabled: Bool {
    @objc(isEnabled) get { return true }
    @objc(setIsEnabled:) set { }
  }

  var isAnimated: Bool = true

  var register: Bool = false
  var this: Properties { return self }

  private(set) var privateSetter = 2
  private(set) var privateSetterCustomNames: Bool {
    @objc(customGetterNameForPrivateSetter) get { return true }
    @objc(customSetterNameForPrivateSetter:) set {}
  }

  static private(set) var privateSetter = 2
  class private(set) var privateSetterCustomNames: Bool {
    @objc(customGetterNameForPrivateSetter) get { return true }
    @objc(customSetterNameForPrivateSetter:) set {}
  }
  static let sharedConstant = 2

  var initContext = 4
  var initContextRO: Int { return 4 }
  var getterIsInit: Bool {
    @objc(initGetter) get { return true }
    set {}
  }
  var setterIsInit: Bool {
    get { return true }
    @objc(initSetter:) set {}
  }

  var customValueTypeProp: URL?
}

// CHECK-LABEL: @interface PropertiesOverridden
// CHECK-NEXT: @property (nonatomic, copy) NSArray<Bee *> * _Nonnull bees;
// CHECK-NEXT: - (null_unspecified instancetype)init
// CHECK-NEXT: - (null_unspecified instancetype)initWithCoder:(NSCoder * _Null_unspecified)aDecoder OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: @end
@objc class PropertiesOverridden : Hive {
  override var bees : [Bee] {
    get {
      return super.bees
    }
    set {
      // Ignore it.
    }
  }
}

// CHECK-LABEL: @interface ReversedOrder2{{$}}
// CHECK-NEXT: init
// CHECK-NEXT: @end
// CHECK: SWIFT_CLASS("_TtC7classes14ReversedOrder1")
// CHECK-NEXT: @interface ReversedOrder1 : ReversedOrder2
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class ReversedOrder1 : ReversedOrder2 {}
@objc class ReversedOrder2 {}


// CHECK-LABEL: @interface Subscripts1
// CHECK-NEXT: - (Subscripts1 * _Nonnull)objectAtIndexedSubscript:(NSInteger)i SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (Subscripts1 * _Nonnull)objectForKeyedSubscript:(Subscripts1 * _Nonnull)o SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class Subscripts1 {
  subscript (i: Int) -> Subscripts1 {
    return self
  }

  subscript (o: Subscripts1) -> Subscripts1 {
    return self
  }
}

// CHECK-LABEL: @interface Subscripts2
// CHECK-NEXT: - (Subscripts2 * _Nonnull)objectAtIndexedSubscript:(int16_t)i SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (void)setObject:(Subscripts2 * _Nonnull)newValue atIndexedSubscript:(int16_t)i;
// CHECK-NEXT: - (NSObject * _Nonnull)objectForKeyedSubscript:(NSObject * _Nonnull)o SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (void)setObject:(NSObject * _Nonnull)newValue forKeyedSubscript:(NSObject * _Nonnull)o;
// CHECK-NEXT: @property (nonatomic, copy) NSArray<NSString *> * _Nonnull cardPaths;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class Subscripts2 {
  subscript (i: Int16) -> Subscripts2 {
    get {
      return self
    }
    set {
      // Ignore it.
    }
  }

  subscript (o: NSObject) -> NSObject {
    get {
      return o
    }
    set {
      // Ignore it.
    }
  }

  // <rdar://problem/17165953> Swift: lazy property reflects back into Objective-C with two properties, one for underlying storage
  lazy var cardPaths : [String] = []
}

// CHECK-LABEL: @interface Subscripts3
// CHECK-NEXT: - (Subscripts3 * _Nonnull)objectAtIndexedSubscript:(unsigned long)_ SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class Subscripts3 {
  subscript (_: CUnsignedLong) -> Subscripts3 {
    return self
  }

  subscript (multi: Int, multi2: Int) -> () {
    return ()
  }
}

// CHECK-LABEL: @interface Throwing1
// CHECK-NEXT: - (BOOL)method1AndReturnError:(NSError * _Nullable * _Nullable)error;
// CHECK-NEXT: - (Throwing1 * _Nullable)method2AndReturnError:(NSError * _Nullable * _Nullable)error SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (NSArray<NSString *> * _Nullable)method3:(NSInteger)x error:(NSError * _Nullable * _Nullable)error SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (nullable instancetype)method4AndReturnError:(NSError * _Nullable * _Nullable)error SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (nullable instancetype)initAndReturnError:(NSError * _Nullable * _Nullable)error OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: - (nullable instancetype)initWithString:(NSString * _Nonnull)string error:(NSError * _Nullable * _Nullable)error OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: - (nullable instancetype)initAndReturnError:(NSError * _Nullable * _Nullable)error fn:(SWIFT_NOESCAPE NSInteger (^ _Nonnull)(NSInteger))fn OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: @end
@objc class Throwing1 {
  func method1() throws { }
  func method2() throws -> Throwing1 { return self }
  func method3(_ x: Int) throws -> [String] { return [] }
  func method4() throws -> Self { return self }

  init() throws { }
  init(string: String) throws { }
  init(fn: (Int) -> Int) throws { }
}

@objc class Spoon: Fungible {}

// CHECK-LABEL: @interface UsesImportedGenerics
@objc class UsesImportedGenerics {
  // CHECK: - (GenericClass<id> * _Nonnull)takeAndReturnGenericClass:(GenericClass<NSString *> * _Nullable)x SWIFT_WARN_UNUSED_RESULT;
  @objc func takeAndReturnGenericClass(_ x: GenericClass<NSString>?) -> GenericClass<AnyObject> { fatalError("") }
  // CHECK: - (FungibleContainer<id <Fungible>> * _Null_unspecified)takeAndReturnFungibleContainer:(FungibleContainer<Spoon *> * _Nonnull)x SWIFT_WARN_UNUSED_RESULT;
  @objc func takeAndReturnFungibleContainer(_ x: FungibleContainer<Spoon>) -> FungibleContainer<Fungible>! { fatalError("") }

  typealias Dipper = Spoon
  // CHECK: - (FungibleContainer<FungibleObject> * _Nonnull)fungibleContainerWithAliases:(FungibleContainer<Spoon *> * _Nullable)x SWIFT_WARN_UNUSED_RESULT;
  @objc func fungibleContainerWithAliases(_ x: FungibleContainer<Dipper>?) -> FungibleContainer<FungibleObject> { fatalError("") }

  // CHECK: - (void)referenceSingleGenericClass:(SingleImportedObjCGeneric<id> * _Nullable)_;
  func referenceSingleGenericClass(_: SingleImportedObjCGeneric<AnyObject>?) {}
}
// CHECK: @end

