// Please keep this file in alphabetical order!

// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift %clang-importer-sdk -enable-source-import -module-cache-path %t/clang-module-cache -emit-module -o %t %s
// RUN: %swift-ide-test %clang-importer-sdk -enable-source-import -module-cache-path %t/clang-module-cache -print-as-objc %t/classes.swiftmodule -source-filename %s > %t/classes.h
// RUN: FileCheck %s < %t/classes.h
// RUN: FileCheck --check-prefix=NEGATIVE %s < %t/classes.h
// RUN: %check-in-clang %t/classes.h

// CHECK-LABEL: @import Foundation;
// CHECK-NEXT: @import swift;
import Foundation

// CHECK-LABEL: @interface A1{{$}}
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class A1 {}

// CHECK-LABEL: @interface B1 : A1
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class B1 : A1 {}

// CHECK-LABEL: @interface Initializers
// CHECK-NEXT: - (instancetype)init;
// CHECK-DISABLED: - (instancetype)initWithInt:(NSInteger)_;
// CHECK-NEXT: - (instancetype)initWithFloat:(float)f;
// CHECK-NEXT: - (instancetype)initWithString:(NSString *)s boolean:(BOOL)b;
// CHECK-NEXT: @end
@objc class Initializers {
  init() {}

  // FIXME: Breaks SILGen! <rdar://problem/15997403>
  //init withInt(_: Int) {}

  init withFloat(f: Float) {}
  init withString(s: String) boolean(b: ObjCBool) {}
}

// NEGATIVE-NOT: NotObjC
class NotObjC {}

// CHECK-LABEL: @interface Methods{{$}}
// CHECK-NEXT: - (void)test;
// CHECK-NEXT: + (void)test2;
// CHECK-NEXT: - (void *)testPrimitives:(BOOL)b i:(NSInteger)i f:(float)f d:(double)d;
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
// CHECK-NEXT: - (instancetype)getSelf;
// CHECK-NEXT: + (SWIFT_METATYPE(Methods))getSelf;
// CHECK-NEXT: - (void)testParens:(NSInteger)a;
// CHECK-NEXT: - (void)testIgnoredParam:(NSInteger)_;
// CHECK-NEXT: - (void)testIgnoredParams:(NSInteger)_ again:(NSInteger)_;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class Methods {
  func test() {}
  type func test2() {}

  func testPrimitives(b: Bool) i(i: Int) f(f: Float) d(d: Double)
    -> COpaquePointer { return COpaquePointer() }
  func testString(s: String) {}
  func testSelector(sel: Selector) boolean(b: ObjCBool) {}

  func testCSignedTypes(a: CSignedChar) b(b: CShort) c(c: CInt) d(d: CLong) e(e: CLongLong) {}
  func testCUnsignedTypes(a: CUnsignedChar) b(b: CUnsignedShort) c(c: CUnsignedInt) d(d: CUnsignedLong) e(e: CUnsignedLongLong) {}
  func testCChars(basic: CChar) wchar(wide: CWideChar) char16(char16: CChar16) char32(char32: CChar32) {}
  func testCFloats(a: CFloat) b(b: CDouble) {}
  func testCBool(a: CBool) {}

  func testSizedSignedTypes(a: Int8) b(b: Int16) c(c: Int32) d(d: Int64) {}
  func testSizedUnsignedTypes(a: UInt8) b(b: UInt16) c(c: UInt32) d(d: UInt64) {}
  func testSizedFloats(a: Float32) b(b: Float64) {}

  func getSelf() -> DynamicSelf { return self }
  type func getSelf() -> Methods.metatype { return self }

  func testParens(a: ((Int))) {}

  func testIgnoredParam(_: Int) {}
  func testIgnoredParams(_: Int) again(_: Int) {}
}

typealias AliasForNSRect = NSRect

// CHECK-LABEL: @interface MethodsWithImports
// CHECK-NEXT: - (NSPoint)getOrigin:(NSRect)r;
// CHECK-NEXT: - (CGFloat)getOriginX:(NSRect)r;
// CHECK-NEXT: - (CGFloat)getOriginY:(CGRect)r;
// CHECK-NEXT: - (NSArray *)emptyArray;
// CHECK-NEXT: - (NSRuncingMode)someEnum;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class MethodsWithImports {
  func getOrigin(r: NSRect) -> NSPoint { return r.origin }
  func getOriginX(r: AliasForNSRect) -> CGFloat { return r.origin.x }
  func getOriginY(r: CGRect) -> CGFloat { return r.origin.y }

  func emptyArray() -> NSArray { return NSArray() }

  func someEnum() -> NSRuncingMode { return .Mince }
}

// CHECK-LABEL: @interface MethodsWithPointers
// CHECK-NEXT: - (id *)test:(NSInteger *)a;
// CHECK-NEXT: - (void)testNested:(NSInteger * *)a;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class MethodsWithPointers {
  func test(a: UnsafePointer<Int>) -> UnsafePointer<AnyObject> { return UnsafePointer() }

  func testNested(a: UnsafePointer<UnsafePointer<Int>>) {}
}

// CHECK-LABEL: @interface MyObject : NSObject
// CHECK-NEXT: init
// CHECK-NEXT: @end
// NEGATIVE-NOT: @interface NSObject
// NEGATIVE-NOT: @class NSObject
class MyObject : NSObject {}

// CHECK-LABEL: @interface Properties
// CHECK-NEXT: @property (nonatomic) NSInteger i;
// CHECK-NEXT: @property (nonatomic, readonly) Properties * this;
// CHECK-NEXT: @property (nonatomic, readonly) double pi;
// CHECK-NEXT: @property (nonatomic) NSInteger computed;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class Properties {
  var i: Int = 1
  var this: Properties {
    return self
  }
  let pi = 3.14
  var computed: Int {
    return 42
  set:
    println("it doesn't change")
  }
}

// CHECK-LABEL: @interface PropertiesOverridden
// CHECK-NEXT: @property (nonatomic, getter=bees, setter=setBees:) NSArray * bees;
// CHECK-NEXT: - (instancetype)init
// CHECK-NEXT: @end
@objc class PropertiesOverridden : Hive {
  var bees : NSArray {
    return super.bees
  set:
    println("can't change that")
  }
}

// CHECK-LABEL: @interface ReversedOrder2{{$}}
// CHECK-NEXT: init
// CHECK-NEXT: @end
// CHECK: @interface ReversedOrder1 : ReversedOrder2
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
// CHECK-NEXT: - (void)setObject:(Subscripts2 *)value atIndexedSubscript:(int16_t)i;
// CHECK-NEXT: - (NSObject *)objectForKeyedSubscript:(NSObject *)o;
// CHECK-NEXT: - (void)setObject:(NSObject *)value forKeyedSubscript:(NSObject *)o;
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class Subscripts2 {
  subscript (i: Int16) -> Subscripts2 {
    return self
  set:
    println("nope")
  }

  subscript (o: NSObject) -> NSObject {
    return o
  set:
    println("nope")
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
