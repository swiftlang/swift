// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/Modules
// RUN: split-file %s %t

// Build the project internal module
// RUN: %target-swift-frontend %t/ProjectInternal.swift \
// RUN:   -emit-module -module-name ProjectInternal \
// RUN:   -o %t/Modules/ProjectInternal.swiftmodule \
// RUN:   -disable-experimental-string-processing

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

//--- ProjectInternal.swift
public class ProjectInternalClass {}

//--- MyModule.swift
@_implementationOnly import ProjectInternal

public protocol PublicProtocol {}

extension ProjectInternalClass : PublicProtocol {}

internal protocol InternalProtocol {
  associatedtype T : PublicProtocol
  static var v : [T] { get }
}

public class PublicClass {}

extension PublicClass : InternalProtocol {
  static let v : [ProjectInternalClass] = []
}
