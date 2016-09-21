// RUN: %target-parse-verify-swift -swift-version 3

#if swift(>=3)
  let w = 1
#else
  // This shouldn't emit any diagnostics.
  asdf asdf asdf asdf
#endif

