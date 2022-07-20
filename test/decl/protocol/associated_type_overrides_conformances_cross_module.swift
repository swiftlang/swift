// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -D M -emit-module -module-name M -parse-as-library -o %t %s
// RUN: %target-typecheck-verify-swift -I %t

#if M

public protocol P {
  associatedtype A
}

public struct S: P {
  public typealias A = Int
}

#else

import M

protocol Q: P {
  associatedtype A // expected-note {{multiple matching types named 'A'}}
}

extension S: Q { // expected-error {{type 'S' does not conform to protocol 'Q'}}
  typealias A = Bool // expected-note {{possibly intended match}}
}

#endif
