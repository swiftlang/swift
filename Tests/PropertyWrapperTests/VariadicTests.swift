import XCTest

final class VariadicPropertyWrapperTests: XCTestCase {
    func testVariadicPropertyWrapper() {
        // Test basic clamping functionality
        process(-10, 50, 200)
        
        // Test with custom range
        processWithRange(5, 30, 60)
    }
    
    // Helper function to verify results
    func verifyResults(_ numbers: [Int], expectedMin: Int, expectedMax: Int) {
        for number in numbers {
            XCTAssertGreaterThanOrEqual(number, expectedMin)
            XCTAssertLessThanOrEqual(number, expectedMax)
        }
    }
}

