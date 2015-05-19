// Please keep this file in alphabetical order!
// Note: this is equivalent to classes.swift, but with Objective-C generics
// REQUIRES: objc_generics

// RUN: rm -rf %t
// RUN: mkdir %t

// FIXME: BEGIN -enable-source-import hackaround
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/objc-generics-sdk -I %t) -emit-module -o %t  %S/../Inputs/objc-generics-sdk/swift-modules/ObjectiveC.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/objc-generics-sdk -I %t) -emit-module -o %t  %S/../Inputs/objc-generics-sdk/swift-modules/CoreGraphics.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/objc-generics-sdk -I %t) -emit-module -o %t  %S/../Inputs/objc-generics-sdk/swift-modules/Foundation.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/objc-generics-sdk -I %t) -emit-module -o %t  %S/../Inputs/objc-generics-sdk/swift-modules/AppKit.swift
// FIXME: END -enable-source-import hackaround


// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/objc-generics-sdk -I %t) -emit-module -o %t  %s
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/objc-generics-sdk -I %t) -parse-as-library %t/classes_objc_generics.swiftmodule -parse -emit-objc-header-path %t/classes.h -import-objc-header %S/../Inputs/empty.h
// RUN: FileCheck %s < %t/classes.h
// RUN: FileCheck --check-prefix=NEGATIVE %s < %t/classes.h
// RUN: %check-in-clang-objc-generics %t/classes.h
// RUN: not %check-in-clang-objc-generics -fno-modules %t/classes.h
// RUN: %check-in-clang-objc-generics -fno-modules %t/classes.h -include Foundation.h -include CoreFoundation.h

// CHECK-NOT: AppKit;
// CHECK-NOT: Properties;
// CHECK-NOT: Swift;
// CHECK-LABEL: @import Foundation;
// CHECK-NEXT: @import CoreGraphics;
// CHECK-NOT: AppKit;
// CHECK-NOT: Swift;
import Foundation
import AppKit // only used in implementations
import CoreFoundation

// CHECK-LABEL: @interface A1{{$}}
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class A1 {}

// CHECK-LABEL: @interface B1 : A1
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class B1 : A1 {}

// CHECK-LABEL: @interface BridgedTypes
// CHECK-NEXT: - (NSDictionary * __nonnull)dictBridge:(NSDictionary * __nonnull)x;
// CHECK-NEXT: - (NSSet * __nonnull)setBridge:(NSSet * __nonnull)x;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class BridgedTypes {
  func dictBridge(x: Dictionary<NSObject, AnyObject>) -> Dictionary<NSObject, AnyObject> {
    return x
  }

  func setBridge(x: Set<NSObject>) -> Set<NSObject> {
    return x
  }
}

// CHECK-LABEL: @interface ClassWithNSObjectProtocol <NSObject>
// CHECK-NEXT: @property (nonatomic, readonly, copy) NSString * __nonnull description;
// CHECK-NEXT: - (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: @end
@objc class ClassWithNSObjectProtocol : NSObjectProtocol {
  var description: String { return "me" }
}

// CHECK-LABEL: @interface Initializers
// CHECK-NEXT: - (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: - (nonnull instancetype)initWithInt:(NSInteger)_;
// CHECK-NEXT: - (nonnull instancetype)initWithFloat:(float)f;
// CHECK-NEXT: - (nonnull instancetype)initWithString:(NSString * __nonnull)s boolean:(BOOL)b;
// CHECK-NEXT: - (nullable instancetype)initWithBoolean:(BOOL)b;
// CHECK-NEXT: - (nonnull instancetype)initForFun OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: @end
@objc class Initializers {
  init() {}

  convenience init(int _: Int) { self.init() }

  convenience init(float f: Float) { self.init() }
  convenience init(string s: String, boolean b: ObjCBool) { self.init() }

  convenience init?(boolean b: ObjCBool) { self.init() }

  init(forFun: ()) { }
}

// NEGATIVE-NOT: NotObjC
class NotObjC {}

// CHECK-LABEL: @interface Methods{{$}}
// CHECK-NEXT: - (void)test;
// CHECK-NEXT: + (void)test2;
// CHECK-NEXT: - (void * __null_unspecified)testPrimitives:(BOOL)b i:(NSInteger)i f:(float)f d:(double)d u:(NSUInteger)u;
// CHECK-NEXT: - (void)testString:(NSString * __nonnull)s;
// CHECK-NEXT: - (void)testSelector:(SEL __null_unspecified)sel boolean:(BOOL)b;
// CHECK-NEXT: - (void)testCSignedTypes:(signed char)a b:(short)b c:(int)c d:(long)d e:(long long)e;
// CHECK-NEXT: - (void)testCUnsignedTypes:(unsigned char)a b:(unsigned short)b c:(unsigned int)c d:(unsigned long)d e:(unsigned long long)e;
// CHECK-NEXT: - (void)testCChars:(char)basic wchar:(wchar_t)wide char16:(char16_t)char16 char32:(char32_t)char32;
// CHECK-NEXT: - (void)testCFloats:(float)a b:(double)b;
// CHECK-NEXT: - (void)testCBool:(bool)a;
// CHECK-NEXT: - (void)testSizedSignedTypes:(int8_t)a b:(int16_t)b c:(int32_t)c d:(int64_t)d;
// CHECK-NEXT: - (void)testSizedUnsignedTypes:(uint8_t)a b:(uint16_t)b c:(uint32_t)c d:(uint64_t)d;
// CHECK-NEXT: - (void)testSizedFloats:(float)a b:(double)b;
// CHECK-NEXT: - (nonnull instancetype)getDynamicSelf;
// CHECK-NEXT: + (SWIFT_METATYPE(Methods) __nonnull)getSelf;
// CHECK-NEXT: - (Methods * __nullable)maybeGetSelf;
// CHECK-NEXT: + (SWIFT_METATYPE(Methods) __nullable)maybeGetSelf;
// CHECK-NEXT: - (Methods * __null_unspecified)uncheckedGetSelf;
// CHECK-NEXT: + (SWIFT_METATYPE(Methods) __null_unspecified)uncheckedGetSelf;
// CHECK-NEXT: - (void)testParens:(NSInteger)a;
// CHECK-NEXT: - (void)testIgnoredParam:(NSInteger)_;
// CHECK-NEXT: - (void)testIgnoredParams:(NSInteger)_ again:(NSInteger)_;
// CHECK-NEXT: - (void)testArrayBridging:(NSArray<Methods *> * __nonnull)a;
// CHECK-NEXT: - (void)testArrayBridging2:(NSArray * __nonnull)a;
// CHECK-NEXT: - (void)testArrayBridging3:(NSArray<NSString *> * __nonnull)a;
// CHECK-NEXT: - (void)testDictionaryBridging:(NSDictionary * __nonnull)a;
// CHECK-NEXT: - (void)testDictionaryBridging2:(NSDictionary<NSNumber *, Methods *> * __nonnull)a;
// CHECK-NEXT: - (void)testDictionaryBridging3:(NSDictionary<NSString *, NSString *> * __nonnull)a;
// CHECK-NEXT: - (void)testSetBridging:(NSSet * __nonnull)a;
// CHECK-NEXT: - (IBAction)actionMethod:(id __nonnull)_;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class Methods {
  func test() {}
  class func test2() {}

  func testPrimitives(b: Bool, i: Int, f: Float, d: Double, u: UInt)
    -> COpaquePointer { return COpaquePointer() }
  func testString(var s: String) {}
  func testSelector(sel: Selector, boolean b: ObjCBool) {}

  func testCSignedTypes(a: CSignedChar, b: CShort, c: CInt, d: CLong, e: CLongLong) {}
  func testCUnsignedTypes(a: CUnsignedChar, b: CUnsignedShort, c: CUnsignedInt, d: CUnsignedLong, e: CUnsignedLongLong) {}
  func testCChars(basic: CChar, wchar wide: CWideChar, char16: CChar16, char32: CChar32) {}
  func testCFloats(a: CFloat, b: CDouble) {}
  func testCBool(a: CBool) {}

  func testSizedSignedTypes(a: Int8, b: Int16, c: Int32, d: Int64) {}
  func testSizedUnsignedTypes(a: UInt8, b: UInt16, c: UInt32, d: UInt64) {}
  func testSizedFloats(a: Float32, b: Float64) {}

  func getDynamicSelf() -> Self { return self }
  class func getSelf() -> Methods.Type { return self }

  func maybeGetSelf() -> Methods? { return nil }
  class func maybeGetSelf() -> Methods.Type? { return self }
  func uncheckedGetSelf() -> Methods! { return self }
  class func uncheckedGetSelf() -> Methods.Type! { return self }

  func testParens(a: ((Int))) {}

  func testIgnoredParam(_: Int) {}
  func testIgnoredParams(_: Int, again _: Int) {}

  func testArrayBridging(a: [Methods]) {}
  func testArrayBridging2(a: [AnyObject]) {}
  func testArrayBridging3(a: [String]) {}

  func testDictionaryBridging(a: [NSObject : AnyObject]) {}
  func testDictionaryBridging2(a: [NSNumber : Methods]) {}
  func testDictionaryBridging3(a: [String : String]) {}

  func testSetBridging(a: Set<NSObject>) {}

  @IBAction func actionMethod(_: AnyObject) {}
}

typealias AliasForNSRect = NSRect

// CHECK-LABEL: @class NSURL;
// NEGATIVE-NOT: @class CFTree
// CHECK-LABEL: @interface MethodsWithImports
// CHECK-NEXT: - (NSPoint)getOrigin:(NSRect)r;
// CHECK-NEXT: - (CGFloat)getOriginX:(NSRect)r;
// CHECK-NEXT: - (CGFloat)getOriginY:(CGRect)r;
// CHECK-NEXT: - (NSArray * __nonnull)emptyArray;
// CHECK-NEXT: - (NSArray * __nullable)maybeArray;
// CHECK-NEXT: - (NSRuncingMode)someEnum;
// CHECK-NEXT: - (NSZone * __null_unspecified)zone;
// CHECK-NEXT: - (CFTypeRef __nullable)cf:(CFTreeRef __nonnull)x str:(CFStringRef __nonnull)str str2:(CFMutableStringRef __nonnull)str2 obj:(CFAliasForTypeRef __nonnull)obj;
// CHECK-NEXT: - (void)appKitInImplementation;
// CHECK-NEXT: - (NSURL * __nullable)returnsURL;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class MethodsWithImports {
  func getOrigin(r: NSRect) -> NSPoint { return r.origin }
  func getOriginX(r: AliasForNSRect) -> CGFloat { return r.origin.x }
  func getOriginY(r: CGRect) -> CGFloat { return r.origin.y }

  func emptyArray() -> NSArray { return NSArray() }
  func maybeArray() -> NSArray? { return nil }

  func someEnum() -> NSRuncingMode { return .Mince }

  func zone() -> NSZone { return nil }

  func cf(x: CFTree, str: CFString, str2: CFMutableString, obj: CFAliasForType) -> CFTypeRef? { return nil }

  func appKitInImplementation() {
    let _ : NSResponder? = nil
  }

  func returnsURL() -> NSURL? { return nil }
}

// CHECK-LABEL: @interface MethodsWithPointers
// CHECK-NEXT: - (id __nonnull * __null_unspecified)test:(NSInteger * __null_unspecified)a;
// CHECK-NEXT: - (void)testNested:(NSInteger * __null_unspecified * __null_unspecified)a;
// CHECK-NEXT: - (void)testBridging:(NSInteger const * __null_unspecified)a b:(NSInteger * __null_unspecified)b c:(Methods * __nonnull * __null_unspecified)c;
// CHECK-NEXT: - (void)testBridgingVoid:(void * __null_unspecified)a b:(void const * __null_unspecified)b;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class MethodsWithPointers {
  func test(a: UnsafeMutablePointer<Int>) -> UnsafeMutablePointer<AnyObject> { return UnsafeMutablePointer() }

  func testNested(a: UnsafeMutablePointer<UnsafeMutablePointer<Int>>) {}

  func testBridging(a: UnsafePointer<Int>, b: UnsafeMutablePointer<Int>, c: AutoreleasingUnsafeMutablePointer<Methods>) {}
  func testBridgingVoid(a: UnsafeMutablePointer<Void>, b: UnsafePointer<Void>) {}
}

// CHECK-LABEL: @interface MyObject : NSObject
// CHECK-NEXT: init
// CHECK-NEXT: @end
// NEGATIVE-NOT: @interface NSObject
class MyObject : NSObject {}

// CHECK-LABEL: @protocol MyProtocol <NSObject>
// CHECK-NEXT: @end
@objc protocol MyProtocol : NSObjectProtocol {}

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
// CHECK-NEXT: @property (nonatomic) Inner2 * __nullable ref2;
// CHECK-NEXT: @property (nonatomic) Inner3 * __nullable ref3;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class NestedMembers {
  // NEGATIVE-NOT: @class NestedMembers;
  // CHECK-LABEL: @interface Inner2
  // CHECK-NEXT: @property (nonatomic) NestedMembers * __nullable ref;
  // CHECK-NEXT: init
  // CHECK-NEXT: @end
  @objc class Inner2 {
    var ref: NestedMembers? = nil
  }

  var ref2: Inner2? = nil
  var ref3: Inner3? = nil

  // CHECK-LABEL: @interface Inner3
  // CHECK-NEXT: @property (nonatomic) NestedMembers * __nullable ref;
  // CHECK-NEXT: init
  // CHECK-NEXT: @end
  @objc class Inner3 {
    var ref: NestedMembers? = nil
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
// CHECK-NEXT: @property (nonatomic, readonly) Properties * __nonnull this;
// CHECK-NEXT: @property (nonatomic, readonly) double pi;
// CHECK-NEXT: @property (nonatomic) NSInteger computed;
// CHECK-NEXT: + (Properties * __nonnull)shared;
// CHECK-NEXT: + (void)setShared:(Properties * __nonnull)newValue;
// CHECK-NEXT: @property (nonatomic, weak) Properties * __nullable weakOther;
// CHECK-NEXT: @property (nonatomic, assign) Properties * __nonnull unownedOther;
// CHECK-NEXT: @property (nonatomic, unsafe_unretained) Properties * __nonnull unmanagedOther;
// CHECK-NEXT: @property (nonatomic, weak) id <MyProtocol> __nullable weakProto;
// CHECK-NEXT: @property (nonatomic) CFTypeRef __nullable weakCF;
// CHECK-NEXT: @property (nonatomic) CFStringRef __nullable weakCFString;
// CHECK-NEXT: @property (nonatomic, weak) IBOutlet id __null_unspecified outlet;
// CHECK-NEXT: @property (nonatomic) IBOutlet Properties * __null_unspecified typedOutlet;
// CHECK-NEXT: @property (nonatomic, copy) NSString * __nonnull string;
// CHECK-NEXT: @property (nonatomic, copy) NSArray * __nonnull array;
// CHECK-NEXT: @property (nonatomic, copy) NSArray * __nonnull arrayOfClasses;
// CHECK-NEXT: @property (nonatomic, copy) NSDictionary<NSString *, NSString *> * __nonnull dictionary;
// CHECK-NEXT: @property (nonatomic, copy) NSDictionary * __nonnull dictStringInt;
// CHECK-NEXT: @property (nonatomic, copy) NSSet<NSString *> * __nonnull stringSet;
// CHECK-NEXT: @property (nonatomic, copy) NSSet * __nonnull intSet;
// CHECK-NEXT: @property (nonatomic, copy) IBOutletCollection(Properties) NSArray<Properties *> * __null_unspecified outletCollection;
// CHECK-NEXT: @property (nonatomic, copy) IBOutletCollection(Properties) NSArray<Properties *> *  __nullable outletCollectionOptional;
// CHECK-NEXT: @property (nonatomic, copy) IBOutletCollection(id) NSArray * __nullable outletCollectionAnyObject;
// CHECK-NEXT: @property (nonatomic, copy) IBOutletCollection(id) NSArray<id <NSObject>> * __nullable outletCollectionProto;
// CHECK-NEXT: + (NSInteger)staticInt;
// CHECK-NEXT: + (NSString * __nonnull)staticString;
// CHECK-NEXT: + (void)setStaticString:(NSString * __nonnull)value;
// CHECK-NEXT: + (double)staticDouble;
// CHECK-NEXT: @property (nonatomic) Properties * __nullable wobble;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class Properties {
  var i: Int = 1
  var this: Properties {
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

  weak var weakOther: Properties?
  unowned var unownedOther: Properties = .shared
  unowned(unsafe) var unmanagedOther: Properties = .shared

  weak var weakProto: MyProtocol?
  weak var weakCF: CFTypeRef?
  weak var weakCFString: CFStringRef?

  @IBOutlet weak var outlet: AnyObject!
  @IBOutlet var typedOutlet: Properties!

  var string = "abc"
  var array: Array<AnyObject> = []
  var arrayOfClasses: Array<NonObjCClass> = []
  var dictionary: Dictionary<String, String> = [:]
  var dictStringInt: Dictionary<String, Int> = [:]
  var stringSet: Set<String> = []
  var intSet: Set<Int> = []

  @IBOutlet var outletCollection: [Properties]!
  @IBOutlet var outletCollectionOptional: [Properties]? = []
  @IBOutlet var outletCollectionAnyObject: [AnyObject]?
  @IBOutlet var outletCollectionProto: [NSObjectProtocol]?

  static let staticInt = 2
  static var staticString = "Hello"
  static var staticDouble: Double {
    return 2.0
  }

  @objc(wobble) var wibble: Properties?
}

// CHECK-LABEL: @interface PropertiesOverridden
// CHECK-NEXT: @property (nonatomic, copy, getter=bees, setter=setBees:) NSArray<Bee *> * __nonnull bees;
// CHECK-NEXT: - (null_unspecified instancetype)init
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
// CHECK: SWIFT_CLASS("_TtC21classes_objc_generics14ReversedOrder1")
// CHECK-NEXT: @interface ReversedOrder1 : ReversedOrder2
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class ReversedOrder1 : ReversedOrder2 {}
@objc class ReversedOrder2 {}


// CHECK-LABEL: @interface Subscripts1
// CHECK-NEXT: - (Subscripts1 * __nonnull)objectAtIndexedSubscript:(NSInteger)i;
// CHECK-NEXT: - (Subscripts1 * __nonnull)objectForKeyedSubscript:(Subscripts1 * __nonnull)o;
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
// CHECK-NEXT: - (Subscripts2 * __nonnull)objectAtIndexedSubscript:(int16_t)i;
// CHECK-NEXT: - (void)setObject:(Subscripts2 * __nonnull)newValue atIndexedSubscript:(int16_t)i;
// CHECK-NEXT: - (NSObject * __nonnull)objectForKeyedSubscript:(NSObject * __nonnull)o;
// CHECK-NEXT: - (void)setObject:(NSObject * __nonnull)newValue forKeyedSubscript:(NSObject * __nonnull)o;
// CHECK-NEXT: @property (nonatomic, copy) NSArray<NSString *> * __nonnull cardPaths;
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
// CHECK-NEXT: - (Subscripts3 * __nonnull)objectAtIndexedSubscript:(unsigned long)_;
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
// CHECK-NEXT: - (BOOL)method1AndReturnError:(NSError * __nullable * __null_unspecified)error;
// CHECK-NEXT: - (Throwing1 * __nullable)method2AndReturnError:(NSError * __nullable * __null_unspecified)error;
// CHECK-NEXT: - (NSArray<NSString *> * __nullable)method3:(NSInteger)x error:(NSError * __nullable * __null_unspecified)error;
// CHECK-NEXT: - (nullable instancetype)method4AndReturnError:(NSError * __nullable * __null_unspecified)error;
// CHECK-NEXT: - (nullable instancetype)initAndReturnError:(NSError * __nullable * __null_unspecified)error OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: - (nullable instancetype)initWithString:(NSString * __nonnull)string error:(NSError * __nullable * __null_unspecified)error OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: - (nullable instancetype)initAndReturnError:(NSError * __nullable * __null_unspecified)error fn:(NSInteger (^ __nonnull)(NSInteger))fn OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: @end
@objc class Throwing1 {
  func method1() throws { }
  func method2() throws -> Throwing1 { return self }
  func method3(x: Int) throws -> [String] { return [] }
  func method4() throws -> Self { return self }

  init() throws { }
  init(string: String) throws { }
  init(fn: Int -> Int) throws { }
}
