// 03_optionals.swift - Optionals and optional handling

// ========== OPTIONALS ==========
let optionalInt: Int? = 42
let nilValue: Int? = nil
let unwrapped = optionalInt!
let optionalChaining = optionalInt?.description
let nilCoalescing = optionalInt ?? 0

// Optional binding
if let value = optionalInt {
    print("Value is \(value)")
}

guard let value = optionalInt else {
    fatalError("No value")
}

// Multiple optional binding
let optionalString: String? = "Hello"
let optionalNumber: Int? = 42

if let str = optionalString, let num = optionalNumber {
    print("Both values: \(str) and \(num)")
}

// Optional chaining with multiple levels
struct Address {
    var street: String?
    var city: String?
}

struct Person {
    var name: String
    var address: Address?
}

let person = Person(name: "John", address: Address(street: "123 Main St", city: "Anytown"))
let city = person.address?.city
let street = person.address?.street

// Implicitly unwrapped optionals
let implicitlyUnwrapped: String! = "Hello"
let unwrappedImplicit = implicitlyUnwrapped // No need for !

// Optional pattern matching
let maybe: Int? = 42
switch maybe {
case .some(let x) where x > 40:
    print("big \(x)")
case .some(let x):
    print("small \(x)")
case .none:
    print("none")
}

// Optional map and flatMap
let optionalArray: [Int]? = [1, 2, 3]
let doubled = optionalArray?.map { $0 * 2 }
let flattened = optionalArray?.flatMap { [$0, $0 * 2] }

print("=== Optionals ===")
print("Unwrapped: \(unwrapped)")
print("Nil coalescing: \(nilCoalescing)")
print("Optional chaining: \(optionalChaining ?? "nil")")
print("City: \(city ?? "unknown")")
print("Doubled: \(doubled ?? [])")
print("Flattened: \(flattened ?? [])")
