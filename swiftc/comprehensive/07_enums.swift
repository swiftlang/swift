// 07_enums.swift - Enumerations and pattern matching

// ========== ENUMERATIONS ==========
// Basic enum
enum Direction: Int, CaseIterable {
    case north = 0
    case south = 1
    case east = 2
    case west = 3
}

// Enum with associated values
enum Barcode {
    case upc(Int, Int, Int, Int)
    case qrCode(String)
}

// Enum with raw values
enum Planet: Int {
    case mercury = 1, venus, earth, mars, jupiter, saturn, uranus, neptune
}

// Recursive enum
enum ArithmeticExpression {
    case number(Int)
    indirect case addition(ArithmeticExpression, ArithmeticExpression)
    indirect case multiplication(ArithmeticExpression, ArithmeticExpression)
}

// Enum with all cases conforming to protocol
enum Direction2: CaseIterable, CustomStringConvertible {
    case north, south, east, west
    
    var description: String {
        switch self {
        case .north: return "North"
        case .south: return "South"
        case .east: return "East"
        case .west: return "West"
        }
    }
}

// Enum with methods
enum Status {
    case success(String)
    case failure(String)
    case loading
    
    func description() -> String {
        switch self {
        case .success(let message):
            return "Success: \(message)"
        case .failure(let error):
            return "Failure: \(error)"
        case .loading:
            return "Loading..."
        }
    }
}

// Enum with computed properties
enum NetworkResponse {
    case success(data: String, statusCode: Int)
    case error(message: String, statusCode: Int)
    
    var isSuccess: Bool {
        switch self {
        case .success:
            return true
        case .error:
            return false
        }
    }
    
    var statusCode: Int {
        switch self {
        case .success(_, let code):
            return code
        case .error(_, let code):
            return code
        }
    }
}

// Pattern matching with enums
func describe(_ barcode: Barcode) -> String {
    switch barcode {
    case .upc(let numberSystem, let manufacturer, let product, let check):
        return "UPC: \(numberSystem), \(manufacturer), \(product), \(check)"
    case .qrCode(let productCode):
        return "QR code: \(productCode)"
    }
}

// Expression patterns
func ~= (pattern: String, value: Int) -> Bool {
    return pattern == "even" ? value % 2 == 0 : value % 2 == 1
}

// Custom pattern matching
switch 42 {
case "even":
    print("Even number")
case "odd":
    print("Odd number")
default:
    print("Unknown")
}

print("=== Enumerations ===")
print("Direction: \(Direction.north)")
print("Planet: \(Planet.earth)")
print("All directions: \(Direction.allCases)")
print("Direction2 description: \(Direction2.north.description)")

let barcode = Barcode.upc(8, 85909, 51226, 3)
print("Barcode: \(describe(barcode))")

let status = Status.success("Operation completed")
print("Status: \(status.description())")

let response = NetworkResponse.success(data: "Sample data", statusCode: 200)
print("Response success: \(response.isSuccess), status: \(response.statusCode)")
