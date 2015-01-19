// Please keep this file in alphabetical order!
// REQUIRES: objc_no_generics

// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend %clang-importer-sdk -enable-source-import -emit-module -o %t %s
// RUN: %target-swift-frontend %clang-importer-sdk -parse-as-library %t/classes.swiftmodule -parse -emit-objc-header-path %t/classes.h -import-objc-header %S/../Inputs/empty.h
// RUN: FileCheck %s < %t/classes.h
// RUN: FileCheck --check-prefix=NEGATIVE %s < %t/classes.h
// RUN: %check-in-clang %t/classes.h
// RUN: not %check-in-clang -fno-modules %t/classes.h
// RUN: %check-in-clang -fno-modules %t/classes.h -include Foundation.h -include ctypes.h -include CoreFoundation.h

// CHECK-NOT: AppKit;
// CHECK-NOT: Properties;
// CHECK-NOT: Swift;
// CHECK-LABEL: @import Foundation;
// CHECK-NEXT: @import CoreGraphics;
// CHECK-NEXT: @import ctypes;
// CHECK-NOT: AppKit;
// CHECK-NOT: Properties;
// CHECK-NOT: Swift;
import Foundation
import AppKit // only used in implementations
import ctypes
import Properties // completely unused
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

// CHECK-LABEL: @interface ClassWithNSObjectProtocol : NSObject <NSObject>
// CHECK-NEXT: - (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: @end
class ClassWithNSObjectProtocol : NSObject, NSObjectProtocol {}

// CHECK-LABEL: @interface Initializers
// CHECK-NEXT: - (SWIFT_NULLABILITY(nonnull) instancetype)init OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: - (SWIFT_NULLABILITY(nonnull) instancetype)initWithInt:(NSInteger)_;
// CHECK-NEXT: - (SWIFT_NULLABILITY(nonnull) instancetype)initWithFloat:(float)f;
// CHECK-NEXT: - (SWIFT_NULLABILITY(nonnull) instancetype)initWithString:(NSString * __nonnull)s boolean:(BOOL)b;
// CHECK-NEXT: - (SWIFT_NULLABILITY(nullable) instancetype)initWithBoolean:(BOOL)b;
// CHECK-NEXT: @end
@objc class Initializers {
  init() {}

  convenience init(int _: Int) { self.init() }

  convenience init(float f: Float) { self.init() }
  convenience init(string s: String, boolean b: ObjCBool) { self.init() }

  convenience init?(boolean b: ObjCBool) { self.init() }
}

// NEGATIVE-NOT: NotObjC
class NotObjC {}

// CHECK-LABEL: @interface Methods{{$}}
// CHECK-NEXT: - (void)test;
// CHECK-NEXT: + (void)test2;
// CHECK-NEXT: - (void *)testPrimitives:(BOOL)b i:(NSInteger)i f:(float)f d:(double)d u:(NSUInteger)u;
// CHECK-NEXT: - (void)testString:(NSString * __nonnull)s;
// CHECK-NEXT: - (void)testSelector:(SEL)sel boolean:(BOOL)b;
// CHECK-NEXT: - (void)testCSignedTypes:(signed char)a b:(short)b c:(int)c d:(long)d e:(long long)e;
// CHECK-NEXT: - (void)testCUnsignedTypes:(unsigned char)a b:(unsigned short)b c:(unsigned int)c d:(unsigned long)d e:(unsigned long long)e;
// CHECK-NEXT: - (void)testCChars:(char)basic wchar:(wchar_t)wide char16:(char16_t)char16 char32:(char32_t)char32;
// CHECK-NEXT: - (void)testCFloats:(float)a b:(double)b;
// CHECK-NEXT: - (void)testCBool:(bool)a;
// CHECK-NEXT: - (void)testSizedSignedTypes:(int8_t)a b:(int16_t)b c:(int32_t)c d:(int64_t)d;
// CHECK-NEXT: - (void)testSizedUnsignedTypes:(uint8_t)a b:(uint16_t)b c:(uint32_t)c d:(uint64_t)d;
// CHECK-NEXT: - (void)testSizedFloats:(float)a b:(double)b;
// CHECK-NEXT: - (SWIFT_NULLABILITY(nonnull) instancetype)getDynamicSelf;
// CHECK-NEXT: + (SWIFT_METATYPE(Methods) __nonnull)getSelf;
// CHECK-NEXT: - (Methods * __nullable)maybeGetSelf;
// CHECK-NEXT: + (SWIFT_METATYPE(Methods) __nullable)maybeGetSelf;
// CHECK-NEXT: - (Methods *)uncheckedGetSelf;
// CHECK-NEXT: + (SWIFT_METATYPE(Methods))uncheckedGetSelf;
// CHECK-NEXT: - (void)testParens:(NSInteger)a;
// CHECK-NEXT: - (void)testIgnoredParam:(NSInteger)_;
// CHECK-NEXT: - (void)testIgnoredParams:(NSInteger)_ again:(NSInteger)_;
// CHECK-NEXT: - (void)testArrayBridging:(NSArray * __nonnull)a;
// CHECK-NEXT: - (void)testArrayBridging2:(NSArray * __nonnull)a;
// CHECK-NEXT: - (void)testArrayBridging3:(NSArray * __nonnull)a;
// CHECK-NEXT: - (void)testDictionaryBridging:(NSDictionary * __nonnull)a;
// CHECK-NEXT: - (void)testDictionaryBridging2:(NSDictionary * __nonnull)a;
// CHECK-NEXT: - (void)testDictionaryBridging3:(NSDictionary * __nonnull)a;
// CHECK-NEXT: - (void)testSetBridging:(NSSet * __nonnull)a;
// CHECK-NEXT: - (IBAction)actionMethod:(id __nonnull)_;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class Methods {
  func test() {}
  class func test2() {}

  func testPrimitives(b: Bool, i: Int, f: Float, d: Double, u: UInt)
    -> COpaquePointer { return COpaquePointer() }
  func testString(s: String) {}
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
// CHECK-NEXT: - (enum NSRuncingMode)someEnum;
// CHECK-NEXT: - (NSZone *)zone;
// CHECK-NEXT: - (struct FooStruct1)tagStruct;
// CHECK-NEXT: - (enum Tribool)tagEnum;
// CHECK-NEXT: - (FooStructTypedef2)anonStructTypedef;
// CHECK-NEXT: - (CFTypeRef)cf:(CFTreeRef)x str:(CFStringRef)str str2:(CFMutableStringRef)str2;
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

  func tagStruct() -> FooStruct1 { return FooStruct1(x: 0, y: 0) }
  func tagEnum() -> Tribool { return True }
  func anonStructTypedef() -> FooStructTypedef2 { return FooStructTypedef2(x: 0, y: 0) }

  func cf(x: CFTree, str: CFString, str2: CFMutableString) -> CFTypeRef? { return nil }

  func appKitInImplementation() {
    let _ : NSResponder? = nil
  }

  func returnsURL() -> NSURL? { return nil }
}

// CHECK-LABEL: @interface MethodsWithPointers
// CHECK-NEXT: - (id __nonnull *)test:(NSInteger *)a;
// CHECK-NEXT: - (void)testNested:(NSInteger * *)a;
// CHECK-NEXT: - (void)testBridging:(NSInteger const *)a b:(NSInteger *)b c:(Methods * __nonnull *)c;
// CHECK-NEXT: - (void)testBridgingVoid:(void *)a b:(void const *)b;
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
// CHECK-NEXT: @property (nonatomic) CFTypeRef weakCF;
// CHECK-NEXT: @property (nonatomic) CFStringRef weakCFString;
// CHECK-NEXT: @property (nonatomic, weak) IBOutlet id outlet;
// CHECK-NEXT: @property (nonatomic) IBOutlet Properties * typedOutlet;
// CHECK-NEXT: @property (nonatomic, copy) NSString * __nonnull string;
// CHECK-NEXT: @property (nonatomic, copy) NSArray * __nonnull array;
// CHECK-NEXT: @property (nonatomic, copy) NSDictionary * __nonnull dictionary;
// CHECK-NEXT: @property (nonatomic, copy) IBOutletCollection(Properties) NSArray * outletCollection;
// CHECK-NEXT: @property (nonatomic, copy) IBOutletCollection(Properties) NSArray * __nullable outletCollectionOptional;
// CHECK-NEXT: @property (nonatomic, copy) IBOutletCollection(id) NSArray * __nullable outletCollectionAnyObject;
// CHECK-NEXT: @property (nonatomic, copy) IBOutletCollection(id) NSArray * __nullable outletCollectionProto;
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
      println("it doesn't change")
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
  var dictionary: Dictionary<String, String> = [:]

  @IBOutlet var outletCollection: [Properties]!
  @IBOutlet var outletCollectionOptional: [Properties]? = []
  @IBOutlet var outletCollectionAnyObject: [AnyObject]?
  @IBOutlet var outletCollectionProto: [NSObjectProtocol]?
}

// CHECK-LABEL: @interface PropertiesOverridden
// CHECK-NEXT: @property (nonatomic, copy, getter=bees, setter=setBees:) NSArray * bees;
// CHECK-NEXT: - (instancetype)init
// CHECK-NEXT: @end
@objc class PropertiesOverridden : Hive {
  override var bees : [AnyObject]! {
    get {
      return super.bees
    }
    set {
      println("can't change that")
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
// CHECK-NEXT: @property (nonatomic, copy) NSArray * __nonnull cardPaths;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class Subscripts2 {
  subscript (i: Int16) -> Subscripts2 {
    get {
      return self
    }
    set {
      println("nope")
    }
  }

  subscript (o: NSObject) -> NSObject {
    get {
      return o
    }
    set {
      println("nope")
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
