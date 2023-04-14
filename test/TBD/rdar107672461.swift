// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/Modules
// RUN: split-file %s %t

// Build the public module
// RUN: %target-swift-frontend %t/Public.swift \
// RUN:   -emit-module -module-name Public \
// RUN:   -o %t/Modules/Public.swiftmodule \
// RUN:   -disable-experimental-string-processing

// Build the project internal module
// RUN: %target-swift-frontend %t/ProjectInternal.swift \
// RUN:   -emit-module -module-name ProjectInternal \
// RUN:   -o %t/Modules/ProjectInternal.swiftmodule \
// RUN:   -I %t/Modules -disable-experimental-string-processing

// Build MyModule with search path to the project internal module
// RUN: %target-swift-frontend %t/MyModule.swift \
// RUN:   -emit-module -module-name MyModule \
// RUN:   -o %t/Modules/MyModule.swiftmodule \
// RUN:   -I %t/Modules -disable-experimental-string-processing

// Remove the project internal module as it's not available to clients of MyModule
// RUN: rm %t/Modules/ProjectInternal.swiftmodule

// Use swift-api-extract to load MyModule without ProjectInternal
// RUN: %target-swift-api-extract -o - -pretty-print \
// RUN:   -module-name MyModule -I %t/Modules

//--- Public.swift
public protocol PublicProtocol {}

//--- ProjectInternal.swift
import Public

public struct InternalStruct: PublicProtocol {}

public extension PublicProtocol {
  func opaque() -> some PublicProtocol {
    return InternalStruct()
  }
}

//--- MyModule.swift
import Public
@_implementationOnly import ProjectInternal

public func foo(bar: some PublicProtocol) -> some PublicProtocol {
  return bar.opaque()
}
