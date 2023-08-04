// REQUIRES: swift_swift_parser
// REQUIRES: rdar113395709

// RUN: %target-swift-frontend -typecheck -parse-as-library -enable-experimental-feature InitAccessors -external-plugin-path %swift-host-lib-dir/plugins#%swift-plugin-server -primary-file %s %S/Inputs/ObservableClass.swift

// RUN: %target-swift-frontend -typecheck -parse-as-library -enable-experimental-feature InitAccessors -external-plugin-path %swift-host-lib-dir/plugins#%swift-plugin-server %s -primary-file %S/Inputs/ObservableClass.swift

// REQUIRES: observation
// REQUIRES: concurrency
// REQUIRES: objc_interop
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

@available(SwiftStdlib 5.9, *)
extension ObservableClass {
  @frozen public enum State: Sendable {
    case unused
    case used
  }
}
