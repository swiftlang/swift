// RUN: %target-typecheck-verify-swift -swift-version 6

// rdar://150177148 - Make sure this does not crash in CSApply when
// argument-matching information for the call is synthesized.
func repeatEachFromClosurePack<each T>(_ t: repeat each T) {
  repeat ({ (arg: repeat each T) in
    each arg
  }())
}

func repeatEachFromOuterPack<each T>(_ t: repeat each T) {
  repeat ({ (arg: repeat each T) in
    each t
  }())
}
