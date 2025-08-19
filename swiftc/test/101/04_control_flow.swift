// 04_control_flow.swift - Control flow statements

// ========== CONTROL FLOW ==========
// If statements
let condition = true
if condition {
    print("True")
} else if !condition {
    print("False")
} else {
    print("Impossible")
}

// Guard statements
func processValue(_ value: Int?) {
    guard let unwrapped = value else {
        print("No value")
        return
    }
    print("Processing \(unwrapped)")
}

// Switch statements
let switchValue = 42
switch switchValue {
case 0:
    print("Zero")
case 1...10:
    print("Small")
case 11...100:
    print("Medium")
case let x where x > 100:
    print("Large: \(x)")
default:
    print("Unknown")
}

// For loops
for i in 1...5 {
    print("Loop iteration \(i)")
}

for i in stride(from: 0, to: 10, by: 2) {
    print("Stride iteration \(i)")
}

for (index, value) in [1, 2, 3, 4, 5].enumerated() {
    print("\(index): \(value)")
}

// While loops
var whileCounter = 0
while whileCounter < 3 {
    print("While iteration \(whileCounter)")
    whileCounter += 1
}

// Repeat-while loops
var repeatCounter = 0
repeat {
    print("Repeat iteration \(repeatCounter)")
    repeatCounter += 1
} while repeatCounter < 3

// Break and continue
for i in 1...10 {
    if i == 5 {
        continue
    }
    if i == 8 {
        break
    }
    print("Number: \(i)")
}

// Labeled statements
outerLoop: for i in 1...3 {
    for j in 1...3 {
        if i == 2 && j == 2 {
            break outerLoop
        }
        print("(\(i), \(j))")
    }
}

// Defer statements
func testDefer() {
    defer {
        print("This runs last")
    }
    print("This runs first")
    print("This runs second")
}

print("=== Control Flow ===")
processValue(42)
testDefer()
