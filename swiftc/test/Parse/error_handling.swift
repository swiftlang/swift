// RUN: %target-typecheck-verify-swift

// Test error handling syntax

enum MyError: Error {
    case invalidInput
    case networkError
}

func throwingFunction() throws -> Int {
    throw MyError.invalidInput
}

func testErrorHandling() {
    do {
        let result = try throwingFunction()
    } catch MyError.invalidInput {
        // Handle invalid input
    } catch MyError.networkError {
        // Handle network error
    } catch {
        // Handle any other error
    }
    
    // Try with optional
    let optional = try? throwingFunction()
    
    // Try with forced unwrapping
    // let forced = try! throwingFunction()
}

func testGuard() {
    guard let value = optional else {
        return
    }
    
    guard value > 0 else {
        throw MyError.invalidInput
    }
}