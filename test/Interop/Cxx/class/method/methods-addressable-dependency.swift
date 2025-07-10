// This ensures that a Swift module built without AddressableParameters can be
// consumed by a dependency Swift module that enables AddressableParameters.

// RUN: rm -rf %t
// RUN: split-file %s %t

// RUN: %target-build-swift %t/library.swift -emit-module -emit-library -enable-library-evolution -cxx-interoperability-mode=upcoming-swift -module-name MyLibrary -emit-module-path %t/artifacts/MyLibrary.swiftmodule -emit-module-interface-path %t/artifacts/MyLibrary.swiftinterface -I %S/Inputs -swift-version 6 -enable-experimental-feature AssumeResilientCxxTypes
// RUN: rm %t/artifacts/MyLibrary.swiftmodule
// RUN: %target-build-swift %t/executable.swift -emit-irgen -cxx-interoperability-mode=default -module-name ImportsMyLibrary -I %t/artifacts -I %S/Inputs -swift-version 6 -enable-experimental-feature AssumeResilientCxxTypes -enable-experimental-feature AddressableParameters

// REQUIRES: swift_feature_AddressableParameters

//--- library.swift
import Methods

@inlinable // emit the body of the function into the textual interface
public func addressableTest(_ x: borrowing NonTrivialInWrapper) {
  let m = HasMethods()
  m.nonTrivialTakesConstRef(x)
}

//--- executable.swift
import MyLibrary
import Methods

let x = NonTrivialInWrapper(123)
let m = HasMethods()
m.nonTrivialTakesConstRef(x)
