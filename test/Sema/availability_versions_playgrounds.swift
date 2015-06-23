// Playgrounds
// RUN: %target-parse-verify-swift -playground

// Script mode
// RUN: %target-parse-verify-swift

// REQUIRES: OS=macosx

func someFunction() {
  // We would normally emit a warning indicating this check is useless (because
  // the minimum deployment target is 10.9) -- but do not when compiling for
  // playgrounds because the developer cannot set the minimum deployment target
  // herself.
  if #available(OSX 10.8, *) {
  }

  if #available(OSX 10.11, *) { // expected-note {{enclosing scope here}}
    // Still warn if the check is useless because an enclosing #available rules
    // it out.
    if #available(OSX 10.11, *) { // expected-warning {{unnecessary check for 'OSX'; enclosing scope ensures guard will always be true}}
    }
  }
}

@available(OSX 10.11, *)
func availableOn10_11() { // expected-note {{enclosing scope here}}
  // Still warn if the check is useless because an enclosing @available rules
  // it out.
  if #available(OSX 10.11, *) { // expected-warning {{unnecessary check for 'OSX'; enclosing scope ensures guard will always be true}}
  }
}


// We also don't want to warn in script mode.
if #available(OSX 10.8, *) {
}
