// 08_structs_classes.swift - Structures and classes

// ========== STRUCTURES & CLASSES ==========
// Structure
struct Point: Equatable {
    var x: Double
    var y: Double
    
    // Computed properties
    var distanceFromOrigin: Double {
        return (x * x + y * y).squareRoot()
    }
    
    // Property observers
    var description: String {
        didSet {
            print("Description changed to: \(description)")
        }
    }
    
    // Initializers
    init(x: Double, y: Double) {
        self.x = x
        self.y = y
        self.description = "Point(\(x), \(y))"
    }
    
    init() {
        self.init(x: 0, y: 0)
    }
    
    // Methods
    func distance(to other: Point) -> Double {
        let dx = x - other.x
        let dy = y - other.y
        return (dx * dx + dy * dy).squareRoot()
    }
    
    // Mutating methods
    mutating func moveBy(x deltaX: Double, y deltaY: Double) {
        x += deltaX
        y += deltaY
    }
    
    // Static methods
    static func origin() -> Point {
        return Point(x: 0, y: 0)
    }
    
    // Static properties
    static let zero = Point(x: 0, y: 0)
}

// Class
class Vehicle {
    // Properties
    var numberOfWheels: Int
    var maxPassengers: Int
    var currentSpeed: Double = 0.0
    
    // Computed properties
    var description: String {
        return "Vehicle with \(numberOfWheels) wheels, max \(maxPassengers) passengers"
    }
    
    // Initializers
    init(wheels: Int, passengers: Int) {
        self.numberOfWheels = wheels
        self.maxPassengers = passengers
    }
    
    // Convenience initializer
    convenience init() {
        self.init(wheels: 4, passengers: 5)
    }
    
    // Methods
    func makeNoise() {
        print("Some noise")
    }
    
    // Final methods
    final func cannotOverride() {
        print("This cannot be overridden")
    }
    
    // Deinitializer
    deinit {
        print("Vehicle is being deinitialized")
    }
}

// Class inheritance
class Car: Vehicle {
    var gear: Int = 1
    
    override func makeNoise() {
        print("Vroom!")
    }
    
    override var description: String {
        return super.description + ", currently in gear \(gear)"
    }
}

// Required initializers
class BaseClass {
    required init() {
        print("Base class init")
    }
}

class SubClass: BaseClass {
    required init() {
        super.init()
        print("Sub class init")
    }
}

// Convenience initializers with chaining
class Food {
    let name: String
    
    init(name: String) {
        self.name = name
    }
    
    convenience init() {
        self.init(name: "[Unnamed]")
    }
}

class RecipeIngredient: Food {
    let quantity: Int
    
    init(name: String, quantity: Int) {
        self.quantity = quantity
        super.init(name: name)
    }
    
    override convenience init(name: String) {
        self.init(name: name, quantity: 1)
    }
}

// Lazy properties
class DataManager {
    lazy var expensiveComputation: String = {
        // Simulate expensive computation
        return "Computed result"
    }()
}

// Property observers with willSet and didSet
class StepCounter {
    var totalSteps: Int = 0 {
        willSet(newTotalSteps) {
            print("About to set totalSteps to \(newTotalSteps)")
        }
        didSet {
            if totalSteps > oldValue {
                print("Added \(totalSteps - oldValue) steps")
            }
        }
    }
}

// Factory methods
class ShapeFactory {
    static func createCircle(radius: Double) -> String {
        return "Circle with radius \(radius)"
    }
    
    static func createCircle(diameter: Double) -> String {
        return "Circle with diameter \(diameter)"
    }
}

// Builder pattern
class StringBuilder {
    private var parts: [String] = []
    
    func append(_ string: String) -> StringBuilder {
        parts.append(string)
        return self
    }
    
    func build() -> String {
        return parts.joined()
    }
}

print("=== Structures & Classes ===")
var point = Point(x: 3, y: 4)
print("Point: \(point.description)")
print("Distance from origin: \(point.distanceFromOrigin)")

let car = Car(wheels: 4, passengers: 5)
print("Car: \(car.description)")
car.makeNoise()

let dataManager = DataManager()
print("Expensive computation: \(dataManager.expensiveComputation)")

let stepCounter = StepCounter()
stepCounter.totalSteps = 100
stepCounter.totalSteps = 150

let builder = StringBuilder().append("Hello").append(" ").append("World").build()
print("Builder result: \(builder)")
