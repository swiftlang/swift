// RUN: %target-run-simple-swift(-parse-as-library -Xfrontend -enable-copy-propagation -Xfrontend -enable-lexical-lifetimes=false  -target %target-swift-5.1-abi-triple) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

struct Container {
    @MainActor static var counter: Int = 10
    @MainActor static var this: Container?

    var noniso: Int = 20

    static func getCount() async -> Int {
        return await counter
    }

    static func getValue() async -> Int? {
        return await this?.noniso
    }
}

@propertyWrapper
struct SuccessTracker {
    private var _stored: Bool
    private var numReads : Int = 0 // an unchecked statistic to exercise mutating get
    init(initially: Bool) {
        self._stored = initially
    }

    var wrappedValue: Bool {
        mutating get {
            numReads += 1
            return _stored
        }
        set { _stored = newValue }
    }
}

func writeToBool(_ b : inout Bool, _ val : Bool) {
    b = val
}

actor List<T : Sendable> {
    var head : T
    var tail : List<T>?

    lazy var hasTail : Bool = (tail != nil)

    var computedTail : List<T>? {
        get { tail }
    }

    subscript(_ offset : Int) -> T? {
        // since we don't have async subscripts, we just return nil for non-zero inputs! :)
        if offset != 0 {
            return nil
        }

        return head
    }

    /// this silly property is here just for testing wrapped-property access
    @SuccessTracker(initially: true) var success : Bool
    func setSuccess(_ b : Bool) { writeToBool(&success, b) }

    init(_ value : T) {
        self.head = value
        self.tail = nil
    }

    init(_ value : T, _ tail : List<T>) {
        self.head = value
        self.tail = tail
    }

    func last() async -> T {
        switch tail {
        case .none:
            return head
        case .some(let tl):
            if await tl.hasTail {
                return await tl.last()
            }
            return await tl.head
        }
    }

    static func tabulate(_ n : Int, _ f : (Int) -> T) -> List<T> {
        if n == 0 {
            return List<T>(f(n))
        }
        return List<T>(f(n), tabulate(n-1, f))
    }

    static func foldr<R>(_ f : (T, R) -> R, _ start : R, _ lst : List<T>) async -> R {
        switch await lst.tail {
            case .none:
                return f(await lst.head, start)
            case .some(let tl):
                return f(await lst.head, await foldr(f, start, tl))
        }
    }
}


actor Tester {
    /// returns true iff success
    func doListTest() async -> Bool {
        let n = 4 // if you change this, you'll have to update other stuff too

        let ints = List<Int>.tabulate(n, { $0 })

        let last1 = await ints.tail!.computedTail!.tail!.tail![0]
        let last2 = await ints.last()

        guard last1 == last2 else {
            print("fail 1")
            return false
        }


        let expected = (n * (n + 1)) / 2
        let sum = await List<Int>.foldr({ $0 + $1 }, 0, ints)

        guard sum == expected else {
            print("fail 2")
            return false
        }

        // CHECK: done list test
        // CHECK-NOT: fail
        print("done list test")
        return true
    }

    func doPropertyWrapperTest() async -> Bool {
        let actor = List<String>("blah")

        await actor.setSuccess(false)
        guard await actor.success == false else {
            print("fail 3")
            return false
        }

        await actor.setSuccess(true)
        guard await actor.success == true else {
            print("fail 4")
            return false
        }

        // CHECK: done property wrapper test
        // CHECK-NOT: fail
        print("done property wrapper test")
        return true
    }

    func doContainerTest() async -> Bool {
        var total: Int = 0
        total += await Container.getCount()
        total += await Container.getValue() ?? 0
        return total == 10
    }
}

@globalActor
struct SillyActor {
    actor _Impl {}
    static let shared = _Impl()
}

@SillyActor
var test : Tester = Tester()

@SillyActor
var expectedResult : Bool {
    get { true }
    set {}
}

@main struct RunIt {
    static func main() async {
        let success = await expectedResult

        guard await test.doListTest() == success else {
            fatalError("fail list test")
        }

        guard await test.doPropertyWrapperTest() == success else {
            fatalError("fail property wrapper test")
        }

        guard await test.doContainerTest() == success else {
            fatalError("fail container test")
        }

        // CHECK: done all testing
        print("done all testing")
    }
}
