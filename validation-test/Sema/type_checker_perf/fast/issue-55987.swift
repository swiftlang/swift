// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000

indirect enum Expression: CustomStringConvertible {
    case integer(Int)
    case real(Double)
    
    case sum(Expression, Expression)
    case difference(Expression, Expression)
    case product(Expression, Expression)
    case division(Expression, Expression)
    
    var description: String {
        switch self {
        case let .integer(a): return "\(a)"
        case let .real(a): return "\(a)"
            
        case let .sum(a, b): return "(\(a) + \(b))"
        case let .difference(a, b): return "(\(a) - \(b))"
        case let .product(a, b): return "(\(a) * \(b))"
        case let .division(a, b): return "(\(a) / \(b))"
        }
    }
}

extension Expression: ExpressibleByIntegerLiteral, ExpressibleByFloatLiteral {
    init(integerLiteral value: IntegerLiteralType) {
        self = .integer(value)
    }
    
    init(floatLiteral value: FloatLiteralType) {
        self = .real(value)
    }
}

extension Expression {
    static func + (lhs: Self, rhs: Self) -> Self { .sum(lhs, rhs) }
    static func - (lhs: Self, rhs: Self) -> Self { .difference(lhs, rhs) }
    static func * (lhs: Self, rhs: Self) -> Self { .product(lhs, rhs) }
    static func / (lhs: Self, rhs: Self) -> Self { .division(lhs, rhs) }
}

func slow() {
  let _: Expression = 3 + 4.1 / 65 * 2 - 12 + 4.1 / 65
}
