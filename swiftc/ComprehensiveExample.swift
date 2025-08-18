// ComprehensiveExample.swift
// No Foundation, no property wrappers, no async/await.

// ---------- Custom operator & precedence ----------
precedencegroup TimesPlusPrecedence { higherThan: AdditionPrecedence }
infix operator **+ : TimesPlusPrecedence
func **+ (lhs: Int, rhs: Int) -> Int { (lhs * rhs) + (lhs + rhs) }

// ---------- Protocols (with associated types) ----------
protocol Container {
    associatedtype Element
    mutating func append(_ element: Element)
    var count: Int { get }
    subscript(_ i: Int) -> Element { get }
}

protocol Reducible {
    associatedtype Element
    func reduce(_ initial: Element, _ combine: (Element, Element) -> Element) -> Element
}

// Constrained extension on Array
extension Array where Element: Comparable {
    func isSorted() -> Bool {
        for i in 1..<count { if self[i-1] > self[i] { return false } }
        return true
    }
}

// ---------- Generic stack that is a Sequence & Container ----------
struct Stack<T>: Sequence, IteratorProtocol, Container, CustomStringConvertible {
    private var storage: [T] = []
    mutating func next() -> T? { storage.popLast() }
    mutating func push(_ x: T) { storage.append(x) }
    @discardableResult mutating func pop() -> T? { storage.popLast() }

    // Container
    mutating func append(_ element: T) { storage.append(element) }
    var count: Int { storage.count }
    subscript(_ i: Int) -> T { storage[i] }

    // Map w/ generic where
    func map<U>(_ f: (T) -> U) -> Stack<U> {
        var out = Stack<U>()
        for x in storage { out.push(f(x)) }
        return out
    }

    var description: String { "Stack(\(storage))" }
}

// ---------- Enums ----------
enum Status: Int { case ok = 0, fail = 1 }

enum Payload {
    case int(Int)
    case text(String)
    case none
}

// ---------- Errors ----------
enum MathError: Error { case negative, overflow }

func sqrtInt(_ x: Int) throws -> Int {
    if x < 0 { throw MathError.negative }
    // toy integer sqrt
    var r = 0
    while (r + 1) * (r + 1) <= x { r += 1 }
    return r
}

// ---------- Inout & closures ----------
func withInout(_ x: inout Int, _ f: (Int) -> Int) { x = f(x) }

func makeAdder(_ base: Int) -> (Int) -> Int {
    return { base + $0 } // escaping closure returned
}

// ---------- Subscript & nested types ----------
struct Matrix {
    let rows: Int, cols: Int
    private var grid: [Int]
    init(_ rows: Int, _ cols: Int, fill: Int = 0) {
        self.rows = rows; self.cols = cols
        self.grid = Array(repeating: fill, count: rows * cols)
    }
    subscript(_ r: Int, _ c: Int) -> Int {
        get { grid[(r * cols) + c] }
        set { grid[(r * cols) + c] = newValue }
    }
    struct Index { let row: Int, col: Int }
}

// ---------- ARC demo ----------
final class Node {
    let id: Int
    weak var next: Node?           // weak to avoid cycle
    unowned var owner: Owner       // unowned back-reference
    init(id: Int, owner: Owner) { self.id = id; self.owner = owner; print("Node \(id) init") }
    deinit { print("Node \(id) deinit") }
}

final class Owner {
    let name: String
    var head: Node?
    init(name: String) { self.name = name; print("Owner \(name) init") }
    deinit { print("Owner \(name) deinit") }
}

// ---------- Access control & typealias ----------
public struct Pair<A, B> {
    public typealias First = A
    public let first: A
    public let second: B
    public init(_ a: A, _ b: B) { self.first = a; self.second = b }
}

// ---------- Pattern matching helpers ----------
func describe(_ p: Payload) -> String {
    switch p {
    case .int(let x) where x % 2 == 0: return "even \(x)"
    case .int(let x): return "odd \(x)"
    case .text(let s): return "text:\(s)"
    case .none: return "none"
    }
}

// ---------- Generic constraints & 'where' ----------
protocol Identity { associatedtype T; func id(_ x: T) -> T }
struct IdentityBox<X>: Identity { func id(_ x: X) -> X { x } }

func allEqual<S: Sequence>(_ s: S) -> Bool where S.Element: Equatable {
    var it = s.makeIterator()
    guard let first = it.next() else { return true }
    while let next = it.next() { if next != first { return false } }
    return true
}

// ---------- @main entry ----------
struct Main {
    static func main() {
        print("--- Operators ---")
        print(3 **+ 4)           // (3*4) + (3+4) = 19

        print("\n--- Generics / Container ---")
        var st = Stack<Int>()
        st.push(1); st.push(2); st.append(3)
        print(st)                // Stack([1, 2, 3])
        let doubled = st.map { $0 * 2 }
        print(doubled)           // Stack([2, 4, 6])

        print("\n--- Protocols with associatedtypes ---")
        let idBox = IdentityBox<String>()
        print(idBox.id("swift"))

        print("\n--- Enums & pattern matching ---")
        print(Status.ok.rawValue)
        print(describe(.int(5)))
        print(describe(.int(12)))
        print(describe(.text("hi")))
        print(describe(.none))

        print("\n--- Error handling ---")
        do {
            print(try sqrtInt(15))   // 3
            _ = try sqrtInt(-1)
            print("unreachable")
        } catch MathError.negative {
            print("caught negative")
        } catch {
            print("caught other error")
        }

        print("\n--- Inout & closures ---")
        var n = 10
        withInout(&n) { $0 + 5 }
        print(n)                 // 15
        let add7 = makeAdder(7)
        print(add7(5))           // 12

        print("\n--- Subscript / nested types ---")
        var m = Matrix(2, 3, fill: 0)
        m[0,0] = 1; m[0,1] = 2; m[0,2] = 3
        m[1,0] = 4; m[1,1] = 5; m[1,2] = 6
        print(m[1,2])            // 6
        let idx = Matrix.Index(row: 1, col: 1)
        print(idx.row, idx.col)  // 1 1

        print("\n--- Extensions with where ---")
        print([1,2,3].isSorted())
        print([3,2,1].isSorted())

        print("\n--- Access control / typealias ---")
        let p = Pair("L", 99)
        print(p.first, p.second)

        print("\n--- Optionals & pattern matching ---")
        let maybe: Int? = 42
        switch maybe {
        case .some(let x) where x > 40: print("big \(x)")
        case .some(let x): print("small \(x)")
        case .none: print("none")
        }

        print("\n--- Sets / Dictionaries / Ranges ---")
        let s: Set<Int> = [1,2,3,3,2,1]
        print(s.contains(2), s.count >= 3)
        let d: [String:Int] = ["a":1, "b":2]
        print(d["a"] ?? -1, d["z"] ?? -1)
        let r = 1...5
        var sum = 0; for i in r { sum += i }
        print(sum)

        print("\n--- ARC demo ---")
        do {
            let owner = Owner(name: "O")
            let a = Node(id: 1, owner: owner)
            let b = Node(id: 2, owner: owner)
            owner.head = a
            a.next = b         // weak
            // Both nodes and owner will deinit when scope ends
        }
        print("ARC scope ended")

        print("\n--- allEqual generic where ---")
        print(allEqual([5,5,5]))
        print(allEqual([5,6,5]))
    }
}

Main.main()


// OUTPUT -----
// --- Operators ---
// 19

// --- Generics / Container ---
// Stack([1, 2, 3])
// Stack([2, 4, 6])

// --- Protocols with associatedtypes ---
// swift

// --- Enums & pattern matching ---
// 0
// odd 5
// even 12
// text:hi
// none

// --- Error handling ---
// 3
// caught negative

// --- Inout & closures ---
// 15
// 12

// --- Subscript / nested types ---
// 6
// 1 1

// --- Extensions with where ---
// true
// false

// --- Access control / typealias ---
// L 99

// --- Optionals & pattern matching ---
// big 42

// --- Sets / Dictionaries / Ranges ---
// true true
// 1 -1
// 15

// --- ARC demo ---
// Owner O init
// Node 1 init
// Node 2 init
// Node 2 deinit
// Owner O deinit
// Node 1 deinit
// ARC scope ended

// --- allEqual generic where ---
// true
// false
