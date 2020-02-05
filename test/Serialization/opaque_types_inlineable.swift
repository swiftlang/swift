// RUN: %target-build-swift -Xfrontend -disable-availability-checking  -module-name A -emit-module %s %S/Inputs/opaque_types_inlineable_2.swift

// This test case use to crash in the merge modules phase when the two partial
// modules are merged as one deserializing the module for this file now has
// access to opaque types in the other file (opaque_types_inlineable_2.swift).

extension P {
  @inlinable
  public func r() -> some P {
    return f { self.o(Q()) }
  }

  @inlinable
  public func q() throws -> some P {
    return try p()
  }
}

public func f<T : P>(_ fn: () -> T) -> some P {
  return K()
}

public struct K : P {
  public init() {}
}

public struct Q : P {
  public init() {}
}
