// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures -disable-requirement-machine-concrete-contraction %s 2>&1 | %FileCheck %s

public protocol P {
  associatedtype A : Q where A.B == Self
}

public protocol Q {
  associatedtype B
}

// This is rejected, because 'A.B == Self' means that 'Self' must
// exactly equal C; since C is not final, this means the conformance
// is not covariant.
public class C : P {
// expected-warning@-1 {{non-final class 'C' cannot safely conform to protocol 'P', which requires that 'Self.A.B' is exactly equal to 'Self'; this is an error in the Swift 6 language mode}}
  public typealias A = D
}

public class D : Q {
  public typealias B = C
}

// This is fine, because FinalC is final.
public final class FinalC : P {
  public typealias A = FinalD
}

public class FinalD : Q {
  public typealias B = FinalC
}

// With the GSB, both <T : P & C> and <T : C & P> minimized to <T where T == C>:
// - T : P and T : C imply that T.A == C.A == D;
// - T : P also implies that T.A.B == T, via A.B == Self in P;
// - Since T.A == D, T.A.B == D.B, therefore Self == D.B.
// - D.B is a typealias for C, so really Self == C.
//
// The Requirement Machine leaves it as <T : C>. Technically this is an ABI break,
// but the entire construction is unsound unless C is final.

// CHECK-LABEL: Generic signature: <T where T : C>
public func takesBoth1<T>(_: T) where T : P, T : C {}

// CHECK-LABEL: Generic signature: <U where U : C>
public func takesBoth2<U>(_: U) where U : C, U : P {}

// 'Self' can also occur inside of a concrete type or superclass requirement.
public class G<T> {}

public protocol ConcreteExampleP {
  associatedtype A : Q where A.B == G<Self>
}

public class ConcreteExampleC : ConcreteExampleP {
// expected-warning@-1 {{non-final class 'ConcreteExampleC' cannot safely conform to protocol 'ConcreteExampleP', which requires that 'Self.A.B' is exactly equal to 'G<Self>'; this is an error in the Swift 6 language mode}}
  public typealias A = ConcreteExampleD
}

public class ConcreteExampleD : Q {
  public typealias B = G<ConcreteExampleC>
}

public protocol SuperclassExampleP {
  associatedtype A : Q where A.B : G<Self>
}

public class SuperclassExampleC : SuperclassExampleP {
// expected-warning@-1 {{non-final class 'SuperclassExampleC' cannot safely conform to protocol 'SuperclassExampleP', which requires that 'Self.A.B' inherit from 'G<Self>'; this is an error in the Swift 6 language mode}}
  public typealias A = SuperclassExampleD
}

public class SuperclassExampleD : Q {
  public typealias B = G<SuperclassExampleC>
}
