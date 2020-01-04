// REQUIRES: VENDOR=apple 
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-stdlib -module-name Swift -validate-tbd-against-ir=all %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-stdlib -module-name Swift -validate-tbd-against-ir=all %s -O

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -parse-stdlib -module-name Swift %s -emit-tbd -emit-tbd-path %t/typecheck.tbd
// RUN: %target-swift-frontend -emit-ir -parse-stdlib -module-name Swift %s -emit-tbd -emit-tbd-path %t/emit-ir.tbd
// RUN: diff -u %t/typecheck.tbd %t/emit-ir.tbd

precedencegroup AssignmentPrecedence {
  assignment: true
  associativity: right
}
precedencegroup FunctionArrowPrecedence {
  associativity: right
  higherThan: AssignmentPrecedence
}
precedencegroup TernaryPrecedence {
  associativity: right
  higherThan: FunctionArrowPrecedence
}
precedencegroup DefaultPrecedence {
  higherThan: TernaryPrecedence
}

infix operator ==: DefaultPrecedence

public struct Bool {
  internal var _value: Builtin.Int1
}

public protocol Equatable {
  static func == (lhs: Self, rhs: Self) -> Bool
}
