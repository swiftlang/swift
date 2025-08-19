// 05_functions.swift - Functions and function types

// ========== FUNCTIONS ==========
// Basic function
func greet(name: String) -> String {
    return "Hello, \(name)!"
}

// Function with default parameters
func greetWithTitle(name: String, title: String = "Mr.") -> String {
    return "Hello, \(title) \(name)!"
}

// Function with variadic parameters
func sum(_ numbers: Int...) -> Int {
    return numbers.reduce(0, +)
}

// Function with inout parameters
func swapValues(_ a: inout Int, _ b: inout Int) {
    let temp = a
    a = b
    b = temp
}

// Function with multiple return values
func minMax(_ array: [Int]) -> (min: Int, max: Int)? {
    guard !array.isEmpty else { return nil }
    return (array.min()!, array.max()!)
}

// Nested functions
func outerFunction() -> Int {
    func innerFunction() -> Int {
        return 42
    }
    return innerFunction()
}

// Function types
let functionVariable: (Int, Int) -> Int = { $0 + $1 }
let result = functionVariable(5, 3)

// Function as parameters
func applyOperation(_ operation: (Int, Int) -> Int, to a: Int, and b: Int) -> Int {
    return operation(a, b)
}

// Function returning functions
func makeAdder(_ base: Int) -> (Int) -> Int {
    return { base + $0 }
}

// Function with generic constraints
func findIndex<T: Equatable>(of valueToFind: T, in array: [T]) -> Int? {
    for (index, value) in array.enumerated() {
        if value == valueToFind {
            return index
        }
    }
    return nil
}

// Function with where clauses
func allItemsMatch<T: Collection>(_ collection: T) -> Bool where T.Element: Equatable {
    guard let first = collection.first else { return true }
    return collection.allSatisfy { $0 == first }
}

// Function with rethrows
func processArray<T>(_ array: [T], _ transform: (T) throws -> String) rethrows -> [String] {
    return try array.map(transform)
}

// Function composition
func compose<A, B, C>(_ f: @escaping (B) -> C, _ g: @escaping (A) -> B) -> (A) -> C {
    return { f(g($0)) }
}

// Partial application
func partial<A, B, C>(_ f: @escaping (A, B) -> C, _ a: A) -> (B) -> C {
    return { b in f(a, b) }
}

print("=== Functions ===")
print(greet(name: "Swift"))
print(greetWithTitle(name: "John"))
print("Sum: \(sum(1, 2, 3, 4, 5))")
print("Function result: \(result)")
print("Apply operation: \(applyOperation(+, to: 5, and: 3))")
print("All items match: \(allItemsMatch([1, 1, 1]))")
print("Find index: \(findIndex(of: 3, in: [1, 2, 3, 4, 5]) ?? -1)")
