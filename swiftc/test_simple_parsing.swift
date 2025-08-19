// Simple test case for parsing
precedencegroup TimesPlusPrecedence { higherThan: AdditionPrecedence }
infix operator **+ : TimesPlusPrecedence
func **+ (lhs: Int, rhs: Int) -> Int { (lhs * rhs) + (lhs + rhs) }

protocol Container {
    associatedtype Element
    mutating func append(_ element: Element)
    var count: Int { get }
}

struct Stack<T> {
    private var storage: [T] = []
    mutating func push(_ x: T) { storage.append(x) }
}

enum Status: Int { 
    case ok = 0
    case fail = 1 
}