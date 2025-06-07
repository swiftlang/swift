// REQUIRES: swift_swift_parser, executable_test

// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -enable-experimental-feature Macros -Xfrontend -plugin-path -Xfrontend %swift-plugin-dir) | %FileCheck %s

// REQUIRES: observation
// REQUIRES: concurrency
// REQUIRES: objc_interop
// REQUIRES: swift_feature_Macros
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Observation

@Observable
public class Model {
  public enum State {
    case initializing
    case running
    case complete
  }

  public var state: State = .initializing {
    willSet {
      print("new state=\(String(describing: newValue))")
    }

    didSet {
      guard oldValue != state else { return }
      print("old state=\(String(describing: oldValue))")
    }
  }
}


let m = Model()

// CHECK: new state=running
// CHECK: old state=initializing
m.state = .running
// CHECK: new state=complete
// CHECK: old state=running
m.state = .complete
