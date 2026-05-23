// RUN: %target-swift-frontend -emit-ir -O -enable-experimental-feature Lifetimes %s

// REQUIRES: swift_feature_Lifetimes

// Minimal reproducer for IRGen crash: rdar://176795176

// MARK: - NE

public struct NE: ~Escapable {
    @_lifetime(immortal)
    public init() {}
}

// MARK: - INES

public protocol INES: ~Escapable {
    @_lifetime(copy self)
    consuming func f(at indices: Range<Int>) -> NE
}

// MARK: - NES

public protocol NES: ~Copyable {
    associatedtype Bytes: INES & ~Escapable
}

// MARK: - IRef

public protocol IRef<PR>: ~Copyable {
    associatedtype PR: ~Copyable
    associatedtype P: BitwiseCopyable

    static func a(of p: P) -> UnsafePointer<PR>
}

// MARK: - Ref

@frozen
public struct Ref<L, C, PR>: ~Escapable & ~Copyable
where
    L: ~Escapable,
    C: ~Copyable & IRef<PR>,
    PR: ~Copyable
{
    @usableFromInline
    let p: C.P

    @_lifetime(immortal)
    @usableFromInline
    init(immortal p: C.P) {
        self.p = p
    }
}

extension Ref: Copyable
where
    L: ~Escapable,
    C: Copyable
{}

extension Ref: BitwiseCopyable
where
    L: ~Escapable,
    C: Copyable
{}

// MARK: - The crashing extension

extension Ref: INES
where
    L: ~Escapable,
    C: Copyable,
    PR: NES & ~Copyable
{
    @_lifetime(copy self)
    public consuming func f(
        at indices: Range<Int>
    ) -> NE {
        let bytes = self.fromPR {
            return _overrideL(immortal: $0[bytes: indices])
        }
        return _overrideL(bytes, copy: self)
    }
}

extension Ref
where
    L: ~Escapable,
    C: ~Copyable,
    PR: ~Copyable
{
    @_lifetime(copy self)
    fileprivate func fromPR<Thrown, Output: ~Escapable>(
        body: (borrowing PR) throws(Thrown) -> Output
    ) throws(Thrown) -> Output
    where
        Thrown: Error,
        Output: ~Copyable
    {
        return _overrideL(
            try body(C.a(of: self.p).pointee),
            copy: self
        )
    }
}

// Subscripts used by f
extension NES where Self: ~Copyable {
    subscript(bytes indices: Range<Int>) -> NE {
        @_lifetime(borrow self)
        get {
            NE()
        }
    }
}

// _overrideL stubs — mimicking stdlib

@_unsafeNonescapableResult
@_lifetime(immortal)
@_alwaysEmitIntoClient
@_transparent
public func _overrideL<T: ~Copyable & ~Escapable>(
    immortal value: consuming T
) -> T {
    value
}

@_unsafeNonescapableResult
@_lifetime(copy source)
@_alwaysEmitIntoClient
@_transparent
public func _overrideL<T: ~Copyable & ~Escapable, U: ~Copyable & ~Escapable>(
    _ value: consuming T,
    copy source: borrowing U
) -> T {
    value
}
