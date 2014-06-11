// Please keep this file in alphabetical order!

// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift %clang-importer-sdk -module-cache-path %t/clang-module-cache -enable-source-import -emit-module -o %t  %s
// RUN: %swift %clang-importer-sdk -module-cache-path %t/clang-module-cache -parse-as-library %t/classes.swiftmodule -parse -emit-objc-header-path %t/classes.h
// RUN: FileCheck %s < %t/classes.h
// RUN: FileCheck --check-prefix=NEGATIVE %s < %t/classes.h
// RUN: %check-in-clang %t/classes.h
// RUN: not %check-in-clang -fno-modules %t/classes.h
// RUN: %check-in-clang -fno-modules %t/classes.h -include Foundation.h -include ctypes.h

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

// CHECK-LABEL: @interface A1{{$}}
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class A1 {}

// CHECK-LABEL: @interface B1 : A1
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class B1 : A1 {}

// CHECK-LABEL: @interface BridgedTypes
// CHECK-NEXT: - (NSDictionary *)dictBridge:(NSDictionary *)x;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class BridgedTypes {
  func dictBridge(x: Dictionary<NSObject, AnyObject>) -> Dictionary<NSObject, AnyObject> { 
    return x 
  }
}

// CHECK-LABEL: @interface ClassWithNSObjectProtocol : NSObject <NSObject>
// CHECK-NEXT: - (instancetype)init OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: @end
class ClassWithNSObjectProtocol : NSObject, NSObjectProtocol {}

// CHECK-LABEL: @interface Initializers
// CHECK-NEXT: - (instancetype)init OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: - (instancetype)initWithInt:(NSInteger)_;
// CHECK-NEXT: - (instancetype)initWithFloat:(float)f;
// CHECK-NEXT: - (instancetype)initWithString:(NSString *)s boolean:(BOOL)b;
// CHECK-NEXT: @end
@objc class Initializers {
  init() {}

  convenience init(int _: Int) { self.init() }

  convenience init(float f: Float) { self.init() }
  convenience init(string s: String, boolean b: ObjCBool) { self.init() }
}

// NEGATIVE-NOT: NotObjC
class NotObjC {}

// CHECK-LABEL: @interface Methods{{$}}
// CHECK-NEXT: - (void)test;
// CHECK-NEXT: + (void)test2;
// CHECK-NEXT: - (void *)testPrimitives:(BOOL)b i:(NSInteger)i f:(float)f d:(double)d u:(NSUInteger)u;
// CHECK-NEXT: - (void)testString:(NSString *)s;
// CHECK-NEXT: - (void)testSelector:(SEL)sel boolean:(BOOL)b;
// CHECK-NEXT: - (void)testCSignedTypes:(signed char)a b:(short)b c:(int)c d:(long)d e:(long long)e;
// CHECK-NEXT: - (void)testCUnsignedTypes:(unsigned char)a b:(unsigned short)b c:(unsigned int)c d:(unsigned long)d e:(unsigned long long)e;
// CHECK-NEXT: - (void)testCChars:(char)basic wchar:(wchar_t)wide char16:(char16_t)char16 char32:(char32_t)char32;
// CHECK-NEXT: - (void)testCFloats:(float)a b:(double)b;
// CHECK-NEXT: - (void)testCBool:(bool)a;
// CHECK-NEXT: - (void)testSizedSignedTypes:(int8_t)a b:(int16_t)b c:(int32_t)c d:(int64_t)d;
// CHECK-NEXT: - (void)testSizedUnsignedTypes:(uint8_t)a b:(uint16_t)b c:(uint32_t)c d:(uint64_t)d;
// CHECK-NEXT: - (void)testSizedFloats:(float)a b:(double)b;
// CHECK-NEXT: - (instancetype)getDynamicSelf;
// CHECK-NEXT: + (SWIFT_METATYPE(Methods))getSelf;
// CHECK-NEXT: - (Methods *)maybeGetSelf;
// CHECK-NEXT: + (SWIFT_METATYPE(Methods))maybeGetSelf;
// CHECK-NEXT: - (Methods *)uncheckedGetSelf;
// CHECK-NEXT: + (SWIFT_METATYPE(Methods))uncheckedGetSelf;
// CHECK-NEXT: - (void)testParens:(NSInteger)a;
// CHECK-NEXT: - (void)testIgnoredParam:(NSInteger)_;
// CHECK-NEXT: - (void)testIgnoredParams:(NSInteger)_ again:(NSInteger)_;
// CHECK-NEXT: - (void)testArrayBridging:(NSArray *)a;
// CHECK-NEXT: - (void)testArrayBridging2:(NSArray *)a;
// CHECK-NEXT: - (void)testArrayBridging3:(NSArray *)a;
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

  func testArrayBridging(a: Methods[]) {}
  func testArrayBridging2(a: AnyObject[]) {}
  func testArrayBridging3(a: String[]) {}
}

typealias AliasForNSRect = NSRect

// CHECK-LABEL: @class NSURL;
// NEGATIVE-NOT: @class CFTree
// CHECK-LABEL: @interface MethodsWithImports
// CHECK-NEXT: - (NSPoint)getOrigin:(NSRect)r;
// CHECK-NEXT: - (CGFloat)getOriginX:(NSRect)r;
// CHECK-NEXT: - (CGFloat)getOriginY:(CGRect)r;
// CHECK-NEXT: - (NSArray *)emptyArray;
// CHECK-NEXT: - (NSArray *)maybeArray;
// CHECK-NEXT: - (NSRuncingMode)someEnum;
// CHECK-NEXT: - (NSZone *)zone;
// CHECK-NEXT: - (CFTypeRef)cf:(CFTreeRef)x str:(CFStringRef)str str2:(CFMutableStringRef)str2;
// CHECK-NEXT: - (void)appKitInImplementation;
// CHECK-NEXT: - (NSURL *)returnsURL;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class MethodsWithImports {
  func getOrigin(r: NSRect) -> NSPoint { return r.origin }
  func getOriginX(r: AliasForNSRect) -> CGFloat { return r.origin.x }
  func getOriginY(r: CGRect) -> CGFloat { return r.origin.y }

  func emptyArray() -> NSArray { return NSArray() }
  func maybeArray() -> NSArray? { return nil }

  func someEnum() -> NSRuncingMode { return .Mince }

  func zone() -> NSZone { return NSZone(pointer: COpaquePointer()) }

  func cf(x: CFTree, str: CFString, str2: CFMutableString) -> CFTypeRef? { return nil }

  func appKitInImplementation() {
    let _ : NSResponder? = nil
  }

  func returnsURL() -> NSURL? { return nil }
}

// CHECK-LABEL: @interface MethodsWithPointers
// CHECK-NEXT: - (id *)test:(NSInteger *)a;
// CHECK-NEXT: - (void)testNested:(NSInteger * *)a;
// CHECK-NEXT: - (void)testBridging:(NSInteger const *)a b:(NSInteger *)b c:(Methods * *)c;
// CHECK-NEXT: - (void)testBridgingVoid:(void *)a b:(void const *)b;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class MethodsWithPointers {
  func test(a: UnsafePointer<Int>) -> UnsafePointer<AnyObject> { return UnsafePointer() }

  func testNested(a: UnsafePointer<UnsafePointer<Int>>) {}

  func testBridging(a: CConstPointer<Int>, b: CMutablePointer<Int>, c: AutoreleasingUnsafePointer<Methods>) {}
  func testBridgingVoid(a: CMutableVoidPointer, b: CConstVoidPointer) {}
}

// CHECK-LABEL: @interface MyObject : NSObject
// CHECK-NEXT: init
// CHECK-NEXT: @end
// NEGATIVE-NOT: @interface NSObject
class MyObject : NSObject {}

// CHECK-LABEL: @protocol MyProtocol <NSObject>
// CHECK-NEXT: @end
@objc protocol MyProtocol : NSObjectProtocol {}

// CHECK-LABEL: @interface Properties
// CHECK-NEXT: @property (nonatomic) NSInteger i;
// CHECK-NEXT: @property (nonatomic, readonly) Properties * this;
// CHECK-NEXT: @property (nonatomic, readonly) double pi;
// CHECK-NEXT: @property (nonatomic) NSInteger computed;
// CHECK-NEXT: @property (nonatomic) Properties * weakOther;
// CHECK-NEXT: @property (nonatomic) Properties * unownedOther;
// CHECK-NEXT: @property (nonatomic) Properties * unmanagedOther;
// CHECK-NEXT: @property (nonatomic) id outlet;
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

  class var shared: Properties { return Properties() }

  weak var weakOther: Properties?
  unowned var unownedOther: Properties = .shared
  unowned(unsafe) var unmanagedOther: Properties = .shared

  @IBOutlet var outlet: AnyObject
}

// CHECK-LABEL: @interface PropertiesOverridden
// CHECK-NEXT: @property (nonatomic, getter=bees, setter=setBees:) NSArray * bees;
// CHECK-NEXT: - (instancetype)init
// CHECK-NEXT: @end
@objc class PropertiesOverridden : Hive {
  override var bees : (AnyObject[])! {
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
// CHECK-NEXT: - (Subscripts1 *)objectAtIndexedSubscript:(NSInteger)i;
// CHECK-NEXT: - (Subscripts1 *)objectForKeyedSubscript:(Subscripts1 *)o;
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
// CHECK-NEXT: - (Subscripts2 *)objectAtIndexedSubscript:(int16_t)i;
// CHECK-NEXT: - (void)setObject:(Subscripts2 *)newValue atIndexedSubscript:(int16_t)i;
// CHECK-NEXT: - (NSObject *)objectForKeyedSubscript:(NSObject *)o;
// CHECK-NEXT: - (void)setObject:(NSObject *)newValue forKeyedSubscript:(NSObject *)o;
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
}

// CHECK-LABEL: @interface Subscripts3
// CHECK-NEXT: - (Subscripts3 *)objectAtIndexedSubscript:(unsigned long)_;
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
