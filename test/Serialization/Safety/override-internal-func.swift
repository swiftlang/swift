/// Deserialization can ignore public overrides to internal methods.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Build the library.
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -I %t \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-path %t/Lib.swiftmodule \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface

/// Build against the swiftmodule.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -enable-deserialization-safety

/// Build against the swiftinterface.
// RUN: rm %t/Lib.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -enable-deserialization-safety

//--- Lib.swift

open class Base {
  internal init() {}

  public func publicMethod() -> Int {
    return 1
  }

  fileprivate func fileprivateMethod() -> Int {
    return 1
  }

  internal func internalMethod() -> Int {
    return 1
  }
}

open class Derived : Base {
  public override init() {
      super.init()
  }

  open override func publicMethod() -> Int {
    return super.publicMethod() + 1
  }

  open override func fileprivateMethod() -> Int {
    return super.fileprivateMethod() + 1
  }

  open override func internalMethod() -> Int {
    return super.internalMethod() + 1
  }
}

//--- Client.swift

import Lib

public class OtherFinalDerived : Derived {
  public override func publicMethod() -> Int {
    return super.publicMethod() + 1
  }

  public override func fileprivateMethod() -> Int {
    return super.fileprivateMethod() + 1
  }

  public override func internalMethod() -> Int {
    return super.internalMethod() + 1
  }
}

func foo() {
    let a = Derived()
}
