// 06_closures.swift - Closures and closure expressions

// ========== CLOSURES ==========
// Closure expressions
let numbers = [1, 2, 3, 4, 5]
let doubled = numbers.map { $0 * 2 }
let filtered = numbers.filter { $0 > 2 }
let reduced = numbers.reduce(0) { $0 + $1 }

// Closure with explicit types
let explicitClosure: (Int) -> Bool = { (value: Int) -> Bool in
    return value > 3
}

// Capturing values
func makeIncrementer(amount: Int) -> () -> Int {
    var runningTotal = 0
    func incrementer() -> Int {
        runningTotal += amount
        return runningTotal
    }
    return incrementer
}

// Escaping closures
func performAsync(completion: @escaping () -> Void) {
    // Simulate async work
    completion()
}

// Autoclosures
func assertIfNeeded(_ condition: @autoclosure () -> Bool, _ message: @autoclosure () -> String) {
    if !condition() {
        print(message())
    }
}

// Capturing lists
var counter = 0
let increment = { [counter] in
    print("Captured counter: \(counter)")
}

// Reentrant closures
func makeReentrantClosure() -> () -> Int {
    var count = 0
    return { count += 1; return count }
}

// Escaping closures with capture lists
class NetworkManager {
    var completionHandlers: [() -> Void] = []
    
    func performRequest(completion: @escaping () -> Void) {
        completionHandlers.append(completion)
    }
}

// Autoclosures with escaping
func performOperation(_ operation: @escaping @autoclosure () -> Bool, completion: @escaping (Bool) -> Void) {
    // Simulate async work
    let result = operation()
    completion(result)
}

// Capture lists with weak and unowned
class DataProcessor {
    var completion: (() -> Void)?
    
    func processData() {
        // Weak capture to avoid retain cycles
        completion = { [weak self] in
            guard let self = self else { return }
            print("Processing data in \(self)")
        }
    }
}

// Capture lists with multiple values
class MultiCapture {
    var value1 = 1
    var value2 = 2
    
    func createClosure() -> () -> Void {
        return { [weak self, value1, value2] in
            guard let self = self else { return }
            print("Values: \(value1), \(value2)")
            print("Self values: \(self.value1), \(self.value2)")
        }
    }
}

// Closure as property
struct Calculator {
    let operation: (Int, Int) -> Int
    
    func calculate(_ a: Int, _ b: Int) -> Int {
        return operation(a, b)
    }
}

// Closure with multiple parameters
let complexClosure: (Int, String, Bool) -> String = { number, text, flag in
    if flag {
        return "\(number): \(text.uppercased())"
    } else {
        return "\(number): \(text.lowercased())"
    }
}

print("=== Closures ===")
print("Doubled: \(doubled)")
print("Filtered: \(filtered)")
print("Reduced: \(reduced)")
print("Explicit closure: \(explicitClosure(5))")
print("Complex closure: \(complexClosure(42, "Hello", true))")

let incrementer = makeIncrementer(amount: 5)
print("Incrementer: \(incrementer())")
print("Incrementer: \(incrementer())")

let calculator = Calculator(operation: { $0 * $1 })
print("Calculator: \(calculator.calculate(6, 7))")
