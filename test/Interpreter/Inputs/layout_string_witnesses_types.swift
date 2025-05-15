import Swift
import CTypes

public class SimpleClass {
    public let x: Int

    public init(x: Int) {
        self.x = x
    }

    deinit {
        print("SimpleClass deinitialized!")
    }
}

public struct SimpleBig {
    let x0: Int = 0
    let x1: Int = 0
    let x2: Int = 0
    let x3: Int = 0
    let x4: Int = 0
    let x5: Int = 0
    let x6: Int = 0
    let x7: Int = 0
    let x8: Int = 0
    let x9: Int = 0

    public init() {}
}

public struct Simple {
    public let x: Int
    public let y: SimpleClass
    public let z: SimpleBig

    public init(x: Int, y: SimpleClass, z: SimpleBig) {
        self.x = x
        self.y = y
        self.z = z
    }
}

public struct CTypeAligned {
    let x = BigAlignment()
    let y: SimpleClass

    public init(_ y: SimpleClass) {
        self.y = y
    }
}

public struct CTypeUnderAligned {
    let w: Int32 = 0
    let x: UnderAligned? = UnderAligned()
    let y: SimpleClass

    public init(_ y: SimpleClass) {
        self.y = y
    }
}

public struct GenericStruct<T> {
    let x: Int = 0
    let y: T

    public init(_ y: T) {
        self.y = y
    }
}

public class GenericClass<T> {
    let x: T

    public init(x: T) {
        self.x = x
    }
}

public struct GenericBig<T> {
    let x: T
    let x1: T
    let x2: T
    let x3: T
    let x4: T
    let x5: T
    let x6: T
    let x7: T
    let x8: T
    let x9: T
    let x10: T

    public init(x: T) {
        self.x = x
        self.x1 = x
        self.x2 = x
        self.x3 = x
        self.x4 = x
        self.x5 = x
        self.x6 = x
        self.x7 = x
        self.x8 = x
        self.x9 = x
        self.x10 = x
    }
}

public struct Generic<A, B, C> {
    public init(x: A, y: GenericClass<B>, z: GenericBig<C>) {
        self.x = x
        self.y = y
        self.z = z
    }

    public let x: A
    public let y: GenericClass<B>
    public let z: GenericBig<C>
}

public struct WeakNativeWrapper {
    public weak var x: SimpleClass?
    let y: Int = 0

    public init(x: SimpleClass) {
        self.x = x
    }
}

public struct UnownedNativeWrapper {
    public unowned let x: SimpleClass
    let y: Int = 0

    public init(x: SimpleClass) {
        self.x = x
    }
}

public struct ClosureWrapper {
    public let f: () -> ()
    let x: Int = 0

    public init(f: @escaping () -> ()) {
        self.f = f
    }
}

public protocol SomeProtocol {}

public struct ExistentialWrapper {
    let x: any SomeProtocol
    let y: Int = 0

    public init(x: any SomeProtocol) {
        self.x = x
    }
}

@inline(never)
public func createExistentialWrapper(_ x: any SomeProtocol) -> ExistentialWrapper {
    return ExistentialWrapper(x: x)
}

public protocol SomeClassProtocol: AnyObject {}

public struct ExistentialRefWrapper {
    let x: any SomeClassProtocol
    let y: Int = 0

    public init(x: any SomeClassProtocol) {
        self.x = x
    }
}

public enum NullableRefEnum {
    case nonEmpty(SimpleClass)
    case empty
}

public enum ForwardToPayloadEnum {
    case nonEmpty(SimpleClass, Int)
    case empty
}

public struct GenericTupleWrapper<T> {
    let x: Int = 23
    let y: (T, Int)

    public init(_ y: (T, Int)) {
        self.y = y
    }
}

public struct GenericNestedOuter<T> {
    public struct Inner {
        let x: Int = 34
        let y: T

        public init(_ y: T) {
            self.y = y
        }
    }
}

public struct GenericNestedRefOuter<T: AnyObject> {
    public struct Inner {
        let x: Int = 34
        let y: T

        public init(_ y: T) {
            self.y = y
        }
    }
}

public enum SimpleEnum {
    case a(AnyObject, Int)
    case b
    case c(Int, AnyObject)
}

struct SimpleEnumWrapper {
    let x: SimpleEnum
    let y: Int = 2
}

public struct GenericEnumWrapper<T> {
    let x: SimpleEnumWrapper
    let y: T

    public init(_ x: SimpleEnum, _ y: T) {
        self.x = SimpleEnumWrapper(x: x)
        self.y = y
    }
}

public struct Recursive3<T> {
    let x: Int
    let y: AnyObject
}

public struct Recursive2<T> {
    let x: Recursive3<Recursive<T>>
    let y: AnyObject
}

public struct Recursive<T> {
    let x: T
    let xs: Recursive2<T>?

    public init(_ x: T, _ xs: Recursive2<T>?) {
        self.x = x
        self.xs = xs
    }
}

public protocol A {}
public protocol B {}
public protocol C {}

public struct MultiProtocolExistentialWrapper {
    let x: Int = 0
    let y: any (A&B&C)
    let z: AnyObject

    public init(y: any (A&B&C), z: AnyObject) {
        self.y = y
        self.z = z
    }
}

public struct AnyWrapper {
    let x: Int = 0
    let y: Any
    let z: AnyObject

    public init(y: Any, z: AnyObject) {
        self.y = y
        self.z = z
    }
}

#if os(macOS)
import Foundation

@objc
public class ObjcClass: NSObject {
    public let x: Int

    public init(x: Int) {
        self.x = x
    }

    deinit {
        print("ObjcClass deinitialized!")
    }
}

public struct ObjcWrapper {
    public let x: ObjcClass
    let y: Int = 0

    public init(x: ObjcClass) {
        self.x = x
    }
}

public struct WeakObjcWrapper {
    public weak var x: ObjcClass?
    let y: Int = 0

    public init(x: ObjcClass) {
        self.x = x
    }
}

public struct UnownedObjcWrapper {
    public unowned let x: ObjcClass
    let y: Int = 0

    public init(x: ObjcClass) {
        self.x = x
    }
}
#endif

public struct Wrapper<T> {
    public let x: T
    let y: Int = 0

    public init(x: T) {
        self.x = x
    }
}

public struct NestedWrapper<T> {
    public let x: Wrapper<T>
    public let y: Wrapper<T>

    public init(x: Wrapper<T>, y: Wrapper<T>) {
        self.x = x
        self.y = y
    }
}

struct InternalGeneric<T> {
    let y: Int
    let x: T
}

public enum SinglePayloadSimpleClassEnum {
    case empty0
    case empty1
    case empty2
    case empty3
    case empty4
    case empty5
    case empty6
    case nonEmpty(SimpleClass)
}

public struct ContainsSinglePayloadSimpleClassEnum {
    public let x: SinglePayloadSimpleClassEnum
    public let y: AnyObject

    public init(x: SinglePayloadSimpleClassEnum, y: AnyObject) {
        self.x = x
        self.y = y
    }
}

public enum TestOptional<T> {
    case empty
    case nonEmpty(T)
}

public enum SinglePayloadEnum<T> {
    case empty
    case nonEmpty(Int, T?)
}

public struct SinglePayloadEnumWrapper<T> {
    let x: SinglePayloadEnum<SinglePayloadEnum<T>>
    let y: Int

    public init(x: SinglePayloadEnum<SinglePayloadEnum<T>>, y: Int) {
        self.x = x
        self.y = y
    }
}

public enum MultiPayloadEnum {
    case a(String, Int)
    case b(Int, String)
    case c(Bool)
    case d
    case e(SimpleClass)
}

public struct MultiPayloadEnumWrapper {
    let x: MultiPayloadEnum
    let y: AnyObject

    public init(x: MultiPayloadEnum, y: AnyObject) {
        self.x = x
        self.y = y
    }
}

public enum MultiPayloadEnumMultiLarge {
    case empty
    case nonEmpty(Int, SimpleClass, Int, SimpleClass, Int, Bool, SimpleClass, Bool, SimpleClass, Bool)
    case nonEmpty2(SimpleClass, Int, Int, SimpleClass, Int, Bool, SimpleClass, Bool, SimpleClass, Bool)
}

public struct MixedEnumWrapper {
    let x: SinglePayloadSimpleClassEnum
    let y: MultiPayloadEnum

    public init(x: SinglePayloadSimpleClassEnum, y: MultiPayloadEnum) {
        self.x = x
        self.y = y
    }
}

public struct MixedEnumWrapperWrapperGeneric<T> {
    let x: MixedEnumWrapper
    let y: T

    public init(x: MixedEnumWrapper, y: T) {
        self.x = x
        self.y = y
    }
}

public struct SinglePayloadEnumExtraTagBytesWrapper {
    let x: SinglePayloadEnumExtraTagBytes
    let y: SimpleClass

    public init(x: SinglePayloadEnumExtraTagBytes, y: SimpleClass) {
        self.x = x
        self.y = y
    }
}

public struct NotBitwiseTakableBridge<T> {
    let x: Int = 0
    let y: [T]
    weak var z: AnyObject? = nil

    public init(_ y: [T]) {
        self.y = y
    }
}

public enum SinglePayloadEnumExtraTagBytes {
    case empty0
    case empty1
    case empty2
    case empty3
    case empty4
    case empty5
    case empty6
    case nonEmpty(WeakNativeWrapper)
}

public struct ComplexNesting<A, B, C, D> {
    let pre: Filler = Filler()
    let a: NestedA<A>
    let b: NestedB<B>
    let c: NestedC<C>
    let d: NestedD<D>

    struct Filler {
        let x: Int16 = 23
        let y: Bool = false
    }

    struct NestedA<T> {
        let x: Int = 32
        let y: NestedB<T>
        let z: Bool = false

        init(y: T) {
            self.y = NestedB(y: y)
        }
    }

    struct NestedB<T> {
        let x: Bool = false
        let y: NestedC<T>
        let z: Int = 32

        init(y: T) {
            self.y = NestedC(y: y)
        }
    }

    enum NestedC<T> {
        case a(Int, T, Bool)
        case b(Int, Bool)
        case c

        init(y: T) {
            self = .a(32, y, false)
        }
    }

    struct NestedD<T> {
        let x: Bool = false
        let y: T
        let z: Int = 32
    }

    public init(_ a: A, _ b: B, _ c: C, _ d: D) {
        self.a = NestedA(y: a)
        self.b = NestedB(y: b)
        self.c = NestedC(y: c)
        self.d = NestedD(y: d)
    }
}

public enum SinglePayloadAnyHashableEnum {
    case empty0
    case empty1
    case empty2
    case empty3
    case nonEmpty(AnyHashable)
}

internal enum InternalEnum {
  case a(Int, AnyObject)
  case b(Int)
  case c(String)
}

public struct InternalEnumWrapper {
  internal let x: InternalEnum
  internal let y: Int = 32

  public init(x: AnyObject) {
    self.x = .a(23, x)
  }
}

public enum SingletonEnum<T> {
    case only(T, Int)
}

public enum SinglePayloadEnumManyXI {
    case empty0
    case empty1
    case empty2
    case empty3
    case empty4
    case empty5
    case empty6
    case nonEmpty(Builtin.Int63, SimpleClass)
}

public struct PrespecializedStruct<T> {
    let y: Int = 0
    let x: T
    let z: T

    public init(x: T) {
        self.x = x
        self.z = x
    }
}

public enum PrespecializedSingletonEnum<T> {
    case only(Int, T)
}

public enum PrespecializedSinglePayloadEnum<T> {
    case empty0
    case empty1
    case nonEmpty(Int, T)
}

public enum PrespecializedMultiPayloadEnum<T> {
    case empty0
    case empty1
    case nonEmpty0(Int, T)
    case nonEmpty1(T, Int)
}

public enum SinglePayloadEnumExistential {
    case a(SomeProtocol, AnyObject)
    case b
    case c
}

public struct TupleLargeAlignment<T> {
    let x: AnyObject? = nil
    let x1: AnyObject? = nil
    let x2: AnyObject? = nil
    let x3: (T, SIMD4<Int64>)

    public init(_ t: T) {
        self.x3 = (t, .init(Int64(Int32.max) + 32, Int64(Int32.max) + 32, Int64(Int32.max) + 32, Int64(Int32.max) + 32))
    }
}

public enum NestedMultiPayloadInner {
    case a(UInt)
    case b(AnyObject)
    case c(AnyObject)
}

public enum NestedMultiPayloadOuter {
    case a(NestedMultiPayloadInner)
    case b(NestedMultiPayloadInner)
    case c(NestedMultiPayloadInner)
}

public enum MultiPayloadError {
    case empty
    case error1(Int, Error)
    case error2(Int, Error)
    case error3(Int, Error)
}

public enum TwoPayloadInner {
    case x(Int)
    case y(AnyObject)
}

public enum TwoPayloadOuter {
    case x(Int)
    case y(TwoPayloadInner)
}

public enum OneExtraTagValue {
    public enum E0 {
        case a(Bool)
        case b(Bool)
    }

    public enum E1 {
        case a(E0)
        case b(Bool)
    }
    public enum E2 {
        case a(E1)
        case b(Bool)
    }
    public enum E3 {
        case a(E2)
        case b(Bool)
    }

    public enum E4 {
        case a(E3)
        case b(Bool)
    }

    case x0(E4, Int8, Int16, Int32)
    case x1(E4, Int8, Int16, Int32)
    case x2(E4, Int8, Int16, Int32)
    case x3(E4, Int8, Int16, Int32)
    case y(SimpleClass)
    case z
}

public enum ErrorWrapper {
    case x(Error)
    case y(Error)
}

public enum MultiPayloadAnyObject {
    case x(AnyObject)
    case y(AnyObject)
    case z(AnyObject)
}

public struct NonCopyableGenericStruct<T>: ~Copyable {
    let x: Int
    let y: T

    public init(x: Int, y: T) {
        self.x = x
        self.y = y
    }
}

public enum NonCopyableGenericEnum<T>: ~Copyable {
    case x(Int, T?)
    case y(Int)
}

@inline(never)
public func consume<T>(_ x: T.Type) {
    withExtendedLifetime(x) {}
}
public func preSpec() {
    consume(PrespecializedStruct<AnyObject>.self)
    consume(PrespecializedStruct<SimpleClass>.self)
    consume(PrespecializedStruct<Int>.self)

    consume(PrespecializedSingletonEnum<AnyObject>.self)
    consume(PrespecializedSingletonEnum<SimpleClass>.self)
    consume(PrespecializedSingletonEnum<Int>.self)

    consume(PrespecializedSinglePayloadEnum<AnyObject>.self)
    consume(PrespecializedSinglePayloadEnum<SimpleClass>.self)
    consume(PrespecializedSinglePayloadEnum<Int>.self)

    consume(PrespecializedMultiPayloadEnum<AnyObject>.self)
    consume(PrespecializedMultiPayloadEnum<SimpleClass>.self)
    consume(PrespecializedMultiPayloadEnum<Int>.self)
}

@inline(never)
public func testAssign<T>(_ ptr: UnsafeMutablePointer<T>, from x: T) {
    ptr.pointee = x
}

@inline(never)
public func testAssignCopy<T>(_ ptr: UnsafeMutablePointer<T>, from x: inout T) {
    ptr.update(from: &x, count: 1)
}

@inline(never)
public func testInit<T>(_ ptr: UnsafeMutablePointer<T>, to x: T) {
    ptr.initialize(to: x)
}

@inline(never)
public func testInitTake<T>(_ ptr: UnsafeMutablePointer<T>, to x: consuming T) {
    ptr.initialize(to: consume x)
}

@inline(never)
public func testDestroy<T>(_ ptr: UnsafeMutablePointer<T>) {
    _ = ptr.move()
}

@inline(never)
public func allocateInternalGenericPtr<T>(of tpe: T.Type) -> UnsafeMutableRawPointer {
    return UnsafeMutableRawPointer(
        UnsafeMutablePointer<InternalGeneric<T>>.allocate(capacity: 1))
}

@inline(never)
public func testGenericAssign<T>(_ ptr: __owned UnsafeMutableRawPointer, from x: T) {
    let ptr = ptr.assumingMemoryBound(to: InternalGeneric<T>.self)
    let x = InternalGeneric(y: 23, x: x)
    testAssign(ptr, from: x)
}

@inline(never)
public func testGenericInit<T>(_ ptr: __owned UnsafeMutableRawPointer, to x: T) {
    let ptr = ptr.assumingMemoryBound(to: InternalGeneric<T>.self)
    let x = InternalGeneric(y: 23, x: x)
    testInit(ptr, to: x)
}

@inline(never)
public func testGenericDestroy<T>(_ ptr: __owned UnsafeMutableRawPointer, of tpe: T.Type) {
    let ptr = ptr.assumingMemoryBound(to: InternalGeneric<T>.self)
    testDestroy(ptr)
}

@inline(never)
public func testGenericArrayDestroy<T>(_ buffer: UnsafeMutableBufferPointer<T>) {
    buffer.deinitialize()
}

@inline(never)
public func testGenericArrayDestroy<T: ~Copyable>(_ buffer: UnsafeMutableBufferPointer<T>) {
    buffer.deinitialize()
}

@inline(never)
public func testGenericArrayInitWithCopy<T>(dest: UnsafeMutableBufferPointer<T>, src: UnsafeMutableBufferPointer<T>) {
    _ = dest.initialize(fromContentsOf: src)
}

@inline(never)
public func testGenericArrayAssignWithCopy<T>(dest: UnsafeMutableBufferPointer<T>, src: UnsafeMutableBufferPointer<T>) {
    _ = dest.update(fromContentsOf: src)
}
