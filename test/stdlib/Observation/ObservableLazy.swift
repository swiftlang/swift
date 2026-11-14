// REQUIRES: swift_swift_parser

// RUN: %target-typecheck-verify-swift -disable-availability-checking -parse-as-library -plugin-path %swift-plugin-dir

// REQUIRES: observation
// REQUIRES: concurrency
// REQUIRES: objc_interop
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Observation

@Observable
final class LazyProperty {
  lazy var value = 0 // expected-error {{'@Observable' does not support 'lazy' properties; mark 'value' with '@ObservationIgnored' to exclude it from observation}}
  // expected-note@-1 {{add '@ObservationIgnored'}} {{3-21=@ObservationIgnored lazy var value = 0}}
}

@Observable
class LazyPropertyWithModifiers {
  var tracked = 0
  // expected-note@+1 {{add '@ObservationIgnored'}}
  private lazy var checkMeLater: String = { // expected-error {{'@Observable' does not support 'lazy' properties; mark 'checkMeLater' with '@ObservationIgnored' to exclude it from observation}}
    String(tracked)
  }()
}

@Observable
final class LazyPropertyExplicitlyTracked {
  @ObservationTracked lazy var value = 0 // expected-error {{'@Observable' does not support 'lazy' properties; mark 'value' with '@ObservationIgnored' to exclude it from observation}}
}

@Observable
final class LazyPropertyIgnored {
  var tracked = 0
  @ObservationIgnored lazy var value = 0
}

func accessLazyIgnored(_ object: LazyPropertyIgnored) -> Int {
  object.tracked + object.value
}
