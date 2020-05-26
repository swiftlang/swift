// REQUIRES: VENDOR=apple 
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s
// RUN: %target-swift-frontend -enable-library-evolution -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -O
// RUN: %target-swift-frontend -enable-library-evolution -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -O

// With -enable-testing:
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-testing
// RUN: %target-swift-frontend -enable-library-evolution -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-testing
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-testing -O
// RUN: %target-swift-frontend -enable-library-evolution -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-testing -O

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -parse-as-library -module-name test %s -emit-tbd -emit-tbd-path %t/typecheck.tbd
// RUN: %target-swift-frontend -emit-ir -parse-as-library -module-name test %s -emit-tbd -emit-tbd-path %t/emit-ir.tbd
// RUN: diff -u %t/typecheck.tbd %t/emit-ir.tbd


public protocol P {}

public class C {
}

public enum SinglePayload: P {
  case A
  case B(C)
  case D
}

public enum MultiPayload {
  case A
  case B(C)
  case D(C)

  public func method() {}
}

public enum AutomaticEquatableHashable {
  case a, b
}

public enum Synthesized: Equatable, Hashable {
  case a(AutomaticEquatableHashable), b
}
public enum ConditionalSynthesized<T> {
  case a(T), b
}

#if swift(>=4)
extension ConditionalSynthesized: Equatable where T: Equatable {}
extension ConditionalSynthesized: Hashable where T: Hashable {}
#endif

public enum ZeroCases {}

public enum OneCase {
  case a
}

public enum OneCasePayload {
  case a(C)
}

public enum PublicEnumDefaultArgument {
  case first(_: Int = 123)
}

internal enum InternalEnumDefaultArgument {
  case first(_: Int = 123)
}
