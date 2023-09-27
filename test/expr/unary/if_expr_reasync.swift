// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-concurrency

// Required for '-enable-experimental-concurrency'
// REQUIRES: concurrency
// REQUIRES: asserts

func asyncFn() async -> Int { 0 }

func reasyncIf1(_ fn: () async -> Int) reasync -> Int {
  let x = if .random() { await fn() } else { 1 }
  return x
}

func reasyncIf2(_ fn: () async -> Int) reasync -> Int {
  if .random() { await fn() } else { 1 }
}

// Not a very good diagnostic, but reasync is still experimental.
func reasyncIf3(_ fn: () async -> Int) reasync -> Int {
  // expected-note@-1 {{add 'async' to function 'reasyncIf3' to make it asynchronous}}
  let x = if .random() { await fn() } else { await asyncFn() }
  // expected-error@-1 {{'async' call in a function that does not support concurrency}}
  return x
}

func reasyncIf4(_ fn: () async -> Int) reasync -> Int {
  let _ = {
    let x = if .random() { await fn() } else { await asyncFn() }
    return x
  }
  return 0
}
