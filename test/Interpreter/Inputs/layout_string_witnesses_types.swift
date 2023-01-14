import Foundation

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

#if os(macOS)
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

struct InternalGeneric<T> {
    let x: T
    let y: Int
}

@inline(never)
public func testAssign<T>(_ ptr: UnsafeMutablePointer<T>, from x: T) {
    ptr.pointee = x
}

@inline(never)
public func testInit<T>(_ ptr: UnsafeMutablePointer<T>, to x: T) {
    ptr.initialize(to: x)
}

@inline(never)
public func testDestroy<T>(_ ptr: UnsafeMutablePointer<T>) {
    ptr.deinitialize(count: 1)
}

@inline(never)
public func allocateInternalGenericPtr<T>(of tpe: T.Type) -> UnsafeMutableRawPointer {
    return UnsafeMutableRawPointer(
        UnsafeMutablePointer<InternalGeneric<T>>.allocate(capacity: 1))
}

@inline(never)
public func testGenericAssign<T>(_ ptr: __owned UnsafeMutableRawPointer, from x: T) {
    let ptr = ptr.assumingMemoryBound(to: InternalGeneric<T>.self)
    let x = InternalGeneric(x: x, y: 23)
    testAssign(ptr, from: x)
}

@inline(never)
public func testGenericInit<T>(_ ptr: __owned UnsafeMutableRawPointer, to x: T) {
    let ptr = ptr.assumingMemoryBound(to: InternalGeneric<T>.self)
    let x = InternalGeneric(x: x, y: 23)
    testInit(ptr, to: x)
}

@inline(never)
public func testGenericDestroy<T>(_ ptr: __owned UnsafeMutableRawPointer, of tpe: T.Type) {
    let ptr = ptr.assumingMemoryBound(to: tpe)
    testDestroy(ptr)
}
