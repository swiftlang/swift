// RUN: %target-typecheck-verify-swift -swift-version 4

#if swift(>=4)
  let w = 1
#else
  // This shouldn't emit any diagnostics.
  asdf asdf asdf asdf
#endif

#if swift(>=4.0)
  let x = 1
#else
  // This shouldn't emit any diagnostics.
  asdf asdf asdf asdf
#endif

#if swift(>=4.0.0)
  let y = 1
#else
  // This shouldn't emit any diagnostics.
  asdf asdf asdf asdf
#endif

#if swift(>=4.0.1)
  // This shouldn't emit any diagnostics.
  asdf asdf asdf asdf
#else
  let z = 1
#endif

