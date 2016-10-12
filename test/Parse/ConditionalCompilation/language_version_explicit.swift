// RUN: %target-parse-verify-swift -swift-version 4

#if swift(>=4)
  let w = 1
#else
  // This shouldn't emit any diagnostics.
  asdf asdf asdf asdf
#endif

