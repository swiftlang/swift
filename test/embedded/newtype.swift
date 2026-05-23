// RUN: %target-typecheck-verify-swift -I %S/Inputs/ -enable-experimental-feature Embedded

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

import StructAsOptionSet

func takeOptionSet<T: OptionSet & Hashable & _SwiftNewtypeWrapper>(_: T) { }

func useStructAsOptionSet() {
  let usage: WGPUBufferUsage = [.mapRead, .mapWrite]
  takeOptionSet(usage)
}
