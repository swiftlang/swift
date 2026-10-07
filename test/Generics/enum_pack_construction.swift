// RUN: %target-typecheck-verify-swift

enum Wrapper<each T> {
    case values(repeat each T)
    case empty
}

func testConstruction() {
    // Explicit type with values
    let w1 = Wrapper.values(1, "hello", true)
    let _: Wrapper<Int, String, Bool> = w1

    // Contextual type
    let w2: Wrapper<Int, String> = .values(42, "world")
    _ = w2

    // Single element pack
    let w3 = Wrapper<Int>.values(42)
    _ = w3

    // Empty case with non-empty pack type
    let w5: Wrapper<Int, String> = .empty
    _ = w5
}

// Test with generic context
func constructGeneric<each T>(_ values: repeat each T) -> Wrapper<repeat each T> {
    return .values(repeat each values)
}

func testGenericConstruction() {
    let w1 = constructGeneric(1, "hello")
    let _: Wrapper<Int, String> = w1

    let w2 = constructGeneric(true)
    let _: Wrapper<Bool> = w2
}

// Test with constraints
enum Constrained<each T: Hashable> {
    case items(repeat each T)
}

func testConstrainedConstruction() {
    let c1 = Constrained.items(1, "hello", true)
    let _: Constrained<Int, String, Bool> = c1
}
