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
}

public func useInit() {
  let _ = CGMutablePath(p: true)
  let _ = CGMutablePath(maybe: true)
  let _ = try! CGMutablePath(toss: true)
}
