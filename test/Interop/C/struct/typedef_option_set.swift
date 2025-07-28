// RUN: %target-typecheck-verify-swift -I %S/Inputs/

import StructAsOptionSet

func takeOptionSet<T: OptionSet>(_: T) { }

func useStructAsOptionSet() {
  let usage: WGPUBufferUsage = [.mapRead, .mapWrite]
  takeOptionSet(usage)
}
