// RUN: %target-typecheck-verify-swift
// REQUIRES: OS=macosx

import CoreGraphics

extension CGMutablePath {
  public convenience init(p: Bool) { // expected-error{{convenience initializers are not supported in extensions of CF types}}
    self.init()
  }
  public convenience init?(maybe: Bool) { // expected-error{{convenience initializers are not supported in extensions of CF types}}
    self.init()
  }

  public convenience init(toss: Bool) throws { // expected-error{{convenience initializers are not supported in extensions of CF types}}
    self.init()
  }

  public init(simple: Bool) { // expected-error{{designated initializer cannot be declared in an extension of 'CGMutablePath'}}{{none}}
                              // expected-error @-1 {{designated initializer for 'CGMutablePath' cannot delegate (with 'self.init')}}{{none}}
    self.init() // expected-note {{delegation occurs here}}
  }

  public init?(value: Bool) { // expected-error{{designated initializer cannot be declared in an extension of 'CGMutablePath'}}{{none}}
                              // expected-error @-1 {{designated initializer for 'CGMutablePath' cannot delegate (with 'self.init')}}{{none}}
    self.init() // expected-note {{delegation occurs here}}
  }

  public init?(string: String) { // expected-error{{designated initializer cannot be declared in an extension of 'CGMutablePath'}}{{none}}
    let _ = string
  }
}

public func useInit() {
  let _ = CGMutablePath(p: true)
  let _ = CGMutablePath(maybe: true)
  let _ = try! CGMutablePath(toss: true)
  let _ = CGMutablePath(simple: true)
  let _ = CGMutablePath(value: true)
  let _ = CGMutablePath(string: "value")
}
