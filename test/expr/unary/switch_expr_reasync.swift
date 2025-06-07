// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-concurrency

// Required for '-enable-experimental-concurrency'
// REQUIRES: concurrency
// REQUIRES: asserts

func asyncFn() async -> Int { 0 }

func reasyncSwitch1(_ fn: () async -> Int) reasync -> Int {
  let x = switch Bool.random() { case true: await fn() case false: 1 }
  return x
}

func reasyncSwitch2(_ fn: () async -> Int) reasync -> Int {
  switch Bool.random() { case true: await fn() case false: 1 }
}

// Not a very good diagnostic, but reasync is still experimental.
func reasyncSwitch3(_ fn: () async -> Int) reasync -> Int {
  // expected-note@-1 {{add 'async' to function 'reasyncSwitch3' to make it asynchronous}}
  let x = switch Bool.random() { case true: await fn() case false: await asyncFn() }
  // expected-error@-1 {{'async' call in a function that does not support concurrency}}
  return x
}

func reasyncSwitch4(_ fn: () async -> Int) reasync -> Int {
  let _ = {
    let x = switch Bool.random() { case true: await fn() case false: await asyncFn() }
    return x
  }
  return 0
}
