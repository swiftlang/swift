// RUN: %target-typecheck-verify-swift

// Basic enum with type pack
enum Wrapper<each T> {
    case values(repeat each T)
    case empty
}

// Enum with type pack and regular generic parameter
enum Result<each T, E: Error> {
    case success(repeat each T)
    case failure(E)
}

// Enum with type pack and constraints
enum Constrained<each T: Hashable> {
    case items(repeat each T)
}

// Enum with multiple cases using the pack
enum Multi<each T> {
    case first(repeat each T)
    case second(repeat each T)
    case none
}

// Nested enum with pack
struct Container<each T> {
    enum Inner {
        case wrapped(repeat each T)
    }
}
