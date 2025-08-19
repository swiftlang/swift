// 09_protocols.swift - Protocols and protocol-oriented programming

// ========== PROTOCOLS ==========
// Basic protocol
protocol Drawable {
    func draw()
    var area: Double { get }
}

// Protocol with associated types
protocol Container {
    associatedtype Item
    var count: Int { get }
    mutating func append(_ item: Item)
    subscript(i: Int) -> Item { get }
}

// Protocol inheritance
protocol Named {
    var name: String { get }
}

protocol Aged {
    var age: Int { get }
}

protocol Person: Named, Aged {
    var occupation: String { get }
}

// Protocol composition
func wishHappyBirthday(to celebrator: Named & Aged) {
    print("Happy birthday \(celebrator.name), you're \(celebrator.age)!")
}

// Protocol extensions
extension Container {
    var isEmpty: Bool {
        return count == 0
    }
}

// Protocol with where clauses
extension Container where Item: Equatable {
    func contains(_ item: Item) -> Bool {
        for i in 0..<count {
            if self[i] == item {
                return true
            }
        }
        return false
    }
}

// Protocol with default implementations
protocol DrawableShape {
    var area: Double { get }
    func draw()
}

extension DrawableShape {
    func draw() {
        print("Drawing shape with area: \(area)")
    }
}

// Protocol composition with where clauses
func processShapes<T: Collection>(_ shapes: T) where T.Element: DrawableShape, T: Sequence {
    for shape in shapes {
        shape.draw()
    }
}

// Protocol with primary associated types
protocol Collection<Element> {
    associatedtype Element
    var count: Int { get }
    func contains(_ element: Element) -> Bool
}

// Protocol inheritance with where clauses
protocol Container2 where Self.Element: Hashable {
    associatedtype Element
    var elements: [Element] { get set }
}

// Protocol with Self requirements
protocol Identifiable {
    associatedtype ID: Hashable
    var id: ID { get }
}

// Generic constraints with protocol composition
func process<T>(_ item: T) where T: Identifiable & CustomStringConvertible {
    print("Processing \(item) with ID: \(item.id)")
}

// Protocol with where clauses
extension Array where Element: Identifiable & Named {
    func findById<T: Hashable>(_ id: T) -> Element? where Element.ID == T {
        return first { $0.id == id }
    }
}

// Protocol with default implementations and where clauses
extension Array where Element: Equatable {
    func allEqual() -> Bool {
        guard let first = first else { return true }
        return allSatisfy { $0 == first }
    }
}

// Protocol with custom operators
protocol Addable {
    static func + (lhs: Self, rhs: Self) -> Self
}

extension Array: Addable where Element: Addable {
    static func + (lhs: Array<Element>, rhs: Array<Element>) -> Array<Element> {
        return lhs + rhs
    }
}

// Protocol with associated type constraints
protocol Storage {
    associatedtype Key: Hashable
    associatedtype Value
    
    func get(_ key: Key) -> Value?
    func set(_ value: Value, for key: Key)
}

// Protocol with type erasure
protocol Animal {
    associatedtype Food
    func eat(_ food: Food)
}

struct AnyAnimal<Food>: Animal {
    private let _eat: (Food) -> Void
    
    init<A: Animal>(_ animal: A) where A.Food == Food {
        self._eat = animal.eat
    }
    
    func eat(_ food: Food) {
        _eat(food)
    }
}

// Protocol with existential types
func processAnimals(_ animals: [any Animal]) {
    for _ in animals {
        // Process each animal
    }
}

print("=== Protocols ===")

// Test basic protocol
struct Circle: Drawable {
    let radius: Double
    var area: Double { return .pi * radius * radius }
    func draw() { print("Drawing circle") }
}

let circle = Circle(radius: 5)
print("Circle area: \(circle.area)")
circle.draw()

// Test protocol composition
struct TestPerson: Named, Aged {
    let name: String
    let age: Int
    let occupation: String
}

let testPerson = TestPerson(name: "Alice", age: 30, occupation: "Developer")
wishHappyBirthday(to: testPerson)

// Test protocol extensions
let numbers = [1, 2, 3, 4, 5]
print("All equal: \(numbers.allEqual())")
print("Contains 3: \(numbers.contains(3))")

// Test protocol with where clauses
struct TestItem: Identifiable, Named {
    let id: String
    let name: String
}

let items = [TestItem(id: "1", name: "Item 1"), TestItem(id: "2", name: "Item 2")]
if let found = items.findById("1") {
    print("Found item: \(found.name)")
}
