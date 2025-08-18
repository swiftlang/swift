// Ultimate Swift Compiler Test - Classes, Protocols, Generics
import Foundation

// Protocol with associated type
protocol Container {
    associatedtype Item
    var count: Int { get }
    mutating func append(_ item: Item)
    subscript(i: Int) -> Item { get }
}

// Generic protocol conformance
protocol Equatable {
    static func ==(lhs: Self, rhs: Self) -> Bool
}

// Generic class with constraints
class Stack<Element: Equatable>: Container {
    private var items: [Element] = []
    
    var count: Int {
        return items.count
    }
    
    func append(_ item: Element) {
        items.append(item)
    }
    
    subscript(i: Int) -> Element {
        return items[i]
    }
    
    func contains(_ item: Element) -> Bool {
        return items.contains { $0 == item }
    }
}

// Protocol extension
extension Container {
    var isEmpty: Bool {
        return count == 0
    }
    
    func first() -> Item? {
        return count > 0 ? self[0] : nil
    }
}

// Generic function with where clause
func findIndex<T: Container>(of valueToFind: T.Item, in container: T) -> Int? 
    where T.Item: Equatable {
    for i in 0..<container.count {
        if container[i] == valueToFind {
            return i
        }
    }
    return nil
}

// Enum with associated values
enum Result<T, E: Error> {
    case success(T)
    case failure(E)
    
    func map<U>(_ transform: (T) -> U) -> Result<U, E> {
        switch self {
        case .success(let value):
            return .success(transform(value))
        case .failure(let error):
            return .failure(error)
        }
    }
}

// Class inheritance
class Animal {
    let name: String
    
    init(name: String) {
        self.name = name
    }
    
    func makeSound() -> String {
        return "Some sound"
    }
}

class Dog: Animal {
    let breed: String
    
    init(name: String, breed: String) {
        self.breed = breed
        super.init(name: name)
    }
    
    override func makeSound() -> String {
        return "Woof!"
    }
    
    func wagTail() {
        print("\(name) is wagging tail")
    }
}

// Protocol with Self requirement
protocol Copyable {
    func copy() -> Self
}

// Generic struct with multiple constraints
struct Pair<T: Equatable & Hashable, U: Comparable> {
    let first: T
    let second: U
    
    init(first: T, second: U) {
        self.first = first
        self.second = second
    }
    
    func isEqual(to other: Pair<T, U>) -> Bool {
        return first == other.first && second == other.second
    }
}

// Usage examples
let stringStack = Stack<String>()
stringStack.append("Hello")
stringStack.append("World")

let dog = Dog(name: "Buddy", breed: "Golden Retriever")
print(dog.makeSound())
dog.wagTail()

let pair = Pair(first: "key", second: 42)
let result: Result<String, Error> = .success("Success!")

// Generic function call
if let index = findIndex(of: "Hello", in: stringStack) {
    print("Found at index \(index)")
}

// Closure with capture
let multiplier = 5
let numbers = [1, 2, 3, 4, 5]
let multiplied = numbers.map { $0 * multiplier }
print("Multiplied: \(multiplied)")

// String interpolation with complex expressions
let message = "Stack has \(stringStack.count) items, dog is \(dog.name)"
print(message)