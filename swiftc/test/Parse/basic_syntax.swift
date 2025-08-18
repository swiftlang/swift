// RUN: %target-typecheck-verify-swift

// Test basic Swift syntax parsing

// Variable declarations
let x = 42
var y = 3.14
let message = "Hello, World!"
var flag = true

// Function declarations
func add(a: Int, b: Int) -> Int {
    return a + b
}

func greet(name: String) {
    let greeting = "Hello, " + name
}

// Control flow
func testControlFlow(x: Int) {
    if x > 0 {
        let positive = true
    } else {
        let negative = false
    }
    
    while x > 0 {
        x = x - 1
    }
    
    for i in 0...10 {
        let value = i * 2
    }
}

// Struct declarations
struct Point {
    let x: Int
    let y: Int
    
    func distance() -> Double {
        return 0.0
    }
}

// Class declarations
class Vehicle {
    var speed: Int
    
    init(speed: Int) {
        self.speed = speed
    }
    
    func accelerate() {
        speed = speed + 10
    }
}

// Enum declarations
enum Direction {
    case north
    case south
    case east
    case west
}

// Switch statements
func testSwitch(direction: Direction) {
    switch direction {
    case .north:
        break
    case .south:
        break
    case .east:
        break
    case .west:
        break
    }
}