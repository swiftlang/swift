// Basic function with default range (0-100)
func process(@Clamped(min: 0, max: 100) _ numbers: Int...) -> [Int] {
    return numbers
}

// Function with custom range
func processWithRange(@Clamped(min: 10, max: 50) _ numbers: Int...) -> [Int] {
    return numbers
}
