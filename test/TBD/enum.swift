// RUN: %target-swift-frontend -emit-ir -o- -parse-as-library -module-name test -validate-tbd-against-ir=missing %s
// RUN: %target-swift-frontend -enable-resilience -emit-ir -o- -parse-as-library -module-name test -validate-tbd-against-ir=missing %s
// Swift 4:
// RUN: %target-swift-frontend -emit-ir -o- -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -swift-version 4
// RUN: %target-swift-frontend -enable-resilience -emit-ir -o- -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -swift-version 4

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
