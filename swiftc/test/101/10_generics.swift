// 10_generics.swift - Generics and generic programming

// ========== GENERICS ==========
// Generic function
func swapTwoValues<T>(_ a: inout T, _ b: inout T) {
    let temporaryA = a
    a = b
    b = temporaryA
}

// Generic type
struct Stack<Element> {
    var items: [Element] = []
    
    mutating func push(_ item: Element) {
        items.append(item)
    }
    
    mutating func pop() -> Element? {
        return items.popLast()
    }
    
    var count: Int {
        return items.count
    }
    
    subscript(i: Int) -> Element {
        return items[i]
    }
}

// Generic type constraints
func findIndex<T: Equatable>(of valueToFind: T, in array: [T]) -> Int? {
    for (index, value) in array.enumerated() {
        if value == valueToFind {
            return index
        }
    }
    return nil
}

// Generic where clauses
func allItemsMatch<C1: Container, C2: Container>(_ someContainer: C1, _ anotherContainer: C2) -> Bool
    where C1.Item == C2.Item, C1.Item: Equatable {
    
    if someContainer.count != anotherContainer.count {
        return false
    }
    
    for i in 0..<someContainer.count {
        if someContainer[i] != anotherContainer[i] {
            return false
        }
    }
    
    return true
}

// Generic constraints with protocol composition
func processItems<T>(_ items: [T]) where T: Identifiable & CustomStringConvertible {
    for item in items {
        print("Processing \(item) with ID: \(item.id)")
    }
}

// Generic constraints with multiple protocols
protocol Identifiable {
    associatedtype ID: Hashable
    var id: ID { get }
}

protocol Named {
    var name: String { get }
}

func processIdentifiableNamed<T>(_ item: T) where T: Identifiable & Named {
    print("Processing \(item.name) with ID: \(item.id)")
}

// Generic where clauses with multiple constraints
extension Array where Element: Identifiable & Named {
    func findById<T: Hashable>(_ id: T) -> Element? where Element.ID == T {
        return first { $0.id == id }
    }
}

// Generic with associated types
protocol Container {
    associatedtype Item
    var count: Int { get }
    mutating func append(_ item: Item)
    subscript(i: Int) -> Item { get }
}

// Generic with Self requirements
protocol Copyable {
    func copy() -> Self
}

// Generic with type constraints
struct GenericPair<T: Comparable, U: Hashable> {
    let first: T
    let second: U
    
    func isFirstGreater() -> Bool {
        return first > first
    }
    
    func hashSecond() -> Int {
        return second.hashValue
    }
}

// Generic with where clauses in extensions
extension Array where Element: Comparable {
    func sorted() -> [Element] {
        return self.sorted(by: <)
    }
    
    func binarySearch(for element: Element) -> Int? {
        var left = 0
        var right = count - 1
        
        while left <= right {
            let mid = (left + right) / 2
            if self[mid] == element {
                return mid
            } else if self[mid] < element {
                left = mid + 1
            } else {
                right = mid - 1
            }
        }
        return nil
    }
}

// Generic with protocol constraints
protocol Drawable {
    func draw()
}

struct GenericDrawer<T: Drawable> {
    let item: T
    
    func drawItem() {
        item.draw()
    }
}

// Generic with multiple type parameters
struct GenericTuple<T, U> {
    let first: T
    let second: U
    
    func swap() -> GenericTuple<U, T> {
        return GenericTuple<U, T>(first: second, second: first)
    }
}

// Generic with type erasure
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

print("=== Generics ===")

// Test generic function
var a = 5
var b = 10
swapTwoValues(&a, &b)
print("Swapped values: \(a), \(b)")

// Test generic type
var stack = Stack<Int>()
stack.push(1)
stack.push(2)
stack.push(3)
print("Stack count: \(stack.count)")
print("Stack pop: \(stack.pop() ?? 0)")

// Test generic constraints
let numbers = [1, 2, 3, 4, 5]
print("Find index of 3: \(findIndex(of: 3, in: numbers) ?? -1)")

// Test generic with where clauses
let sortedNumbers = numbers.sorted()
print("Sorted numbers: \(sortedNumbers)")
print("Binary search for 3: \(sortedNumbers.binarySearch(for: 3) ?? -1)")

// Test generic with protocol constraints
struct TestItem: Identifiable, Named {
    let id: String
    let name: String
}

let items = [TestItem(id: "1", name: "Item 1"), TestItem(id: "2", name: "Item 2")]
if let found = items.findById("1") {
    print("Found item: \(found.name)")
}

// Test generic tuple
let tuple = GenericTuple(first: "Hello", second: 42)
let swapped = tuple.swap()
print("Swapped tuple: \(swapped.first), \(swapped.second)")
