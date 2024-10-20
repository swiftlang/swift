// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -o %t/a.swiftmodule -emit-module-source-info-path %t/a.swiftsourceinfo -primary-file %s %S/Inputs/actor_bar.swift -module-name Foo -target %target-swift-5.1-abi-triple
// RUN: %target-swift-frontend -emit-module -o %t/b.swiftmodule -emit-module-source-info-path %t/b.swiftsourceinfo -primary-file %S/Inputs/actor_bar.swift %s -module-name Foo -target %target-swift-5.1-abi-triple
// RUN: %target-swift-frontend -merge-modules -emit-module -o %t/Foo.swiftmodule  -emit-module-source-info-path %t/Foo.swiftsourceinfo %t/a.swiftmodule %t/b.swiftmodule -module-name Foo -target %target-swift-5.1-abi-triple

// REQUIRES: concurrency

extension Bar {
  @MainActor
  func bar() async throws -> Int {
    return 42
  }
}
