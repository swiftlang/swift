// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Build a Swift library that uses symbols from a C library without enabling C++ interop.
// RUN: %target-build-swift %t/uses-c-library.swift -emit-module -emit-library -enable-library-evolution -module-name UsesCLibrary -emit-module-path %t/artifacts/UsesCLibrary.swiftmodule -emit-module-interface-path %t/artifacts/UsesCLibrary.swiftinterface -I %S/Inputs

// Make sure the module interface can be type-checked with C++ interop enabled.
// RUN: %target-swift-frontend -typecheck-module-from-interface -cxx-interoperability-mode=default %t/artifacts/UsesCLibrary.swiftinterface -I %S/Inputs

// Make sure we can build a Swift executable that uses the library and enables C++ interop.
// RUN: %target-swift-frontend -typecheck -cxx-interoperability-mode=default -module-name Main %t/main.swift -I %t/artifacts -I %S/Inputs

//--- uses-c-library.swift

import MyCLibrary

public func getMyCStruct() -> MyCStruct {
  return MyCStruct()
}

extension MyCStruct {
  @inlinable public var y: CInt {
    get {
      return self.x
    }
  }

  @inlinable public var anotherInstanceOfSelf: MyCStruct {
    get {
      return MyCStruct(x: self.x + 1)
    }
  }
}

public func getMyCEnum() -> MyCEnum {
  return MCE_One
}

public func getMyCFixedEnum() -> MyCFixedEnum {
  return MCFE_One
}

//--- main.swift

import UsesCLibrary

let _ = getMyCStruct()
let _ = getMyCStruct().y
let _ = getMyCStruct().anotherInstanceOfSelf
let _ = getMyCEnum()
let _ = getMyCFixedEnum()
