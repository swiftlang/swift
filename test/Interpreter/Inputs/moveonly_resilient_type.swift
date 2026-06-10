struct MyError: Error {}

public struct Resilient: ~Copyable {
    static var nextValue: Int = 0

    private(set) public var value: Int
    public init(nonthrowing: ()) {
        value = Self.nextValue
        Self.nextValue += 1
    }
    deinit { print("resilient deinit \(value)") }
    
    public init(throwing: Bool) throws {
        if throwing {
            throw MyError()
        }
        self = .init(nonthrowing: ())
    }
    public init(throwingAfterInit: Bool) throws {
        self = .init(nonthrowing: ())
        if throwingAfterInit {
            throw MyError()
        }
    }

    public static func instanceCount() -> Int {
        return nextValue
    }
}

func testCapture(_: () -> Bool) {}

public struct ResilientCapturesInDeinit: ~Copyable {
    static var nextValue: Int = 0

    private(set) public var value: Int
    public init(nonthrowing: ()) {
        value = Self.nextValue
        Self.nextValue += 1
    }
    deinit {
        testCapture { value >= 0 }
        print("resilient capture in deinit \(value)")
    }
    
    public init(throwing: Bool) throws {
        if throwing {
            throw MyError()
        }
        self = .init(nonthrowing: ())
    }
    public init(throwingAfterInit: Bool) throws {
        self = .init(nonthrowing: ())
        if throwingAfterInit {
            throw MyError()
        }
    }

    public static func instanceCount() -> Int {
        return nextValue
    }
}

public struct HasDeinit: ~Copyable {
  public consuming func consume() {}
  deinit { print("HasDeinit.deinit") }
}

public protocol Giver {
  @_owned var thing: HasDeinit { get }
}

public struct GiverImpl: Giver {
  public init() {}
  @_owned public var thing: HasDeinit {
    get { return HasDeinit() }
  }
}
