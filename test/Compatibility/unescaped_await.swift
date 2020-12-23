// RUN: %target-typecheck-verify-swift
    // now check that the fix-its, if applied, will fix the warnings.
// RUN: %empty-directory(%t.scratch)
// RUN: cp %s %t.scratch/fixits.swift
// RUN: %target-swift-frontend -typecheck %t.scratch/fixits.swift -fixit-all -emit-fixits-path %t.scratch/fixits.remap 2> /dev/null
// RUN: %{python} %utils/apply-fixit-edits.py %t.scratch
// RUN: %target-swift-frontend -typecheck %t.scratch/fixits.swift -warnings-as-errors
// RUN: %target-swift-frontend -typecheck %t.scratch/fixits.swift -warnings-as-errors -enable-experimental-concurrency

// REQUIRES: concurrency

func await(_ f : () -> Void) {}

func ordinaryCalls() {
  await({})
  // expected-warning@-1 {{future versions of Swift reserve the word 'await'; if this name is unavoidable, use backticks to escape it}}

  await {}
  // expected-warning@-1 {{future versions of Swift reserve the word 'await'; if this name is unavoidable, use backticks to escape it}}

  let _ = `await`
  let _ = `await`({})
  
  let _ = await
  // expected-warning@-1 {{future versions of Swift reserve the word 'await'; if this name is unavoidable, use backticks to escape it}}

  let k = Klass()
  k.await()
  _ = k.await
}

func localVar() {
  var await = 1

  let two = await + await
  // expected-warning@-1 2 {{future versions of Swift reserve the word 'await'; if this name is unavoidable, use backticks to escape it}}
  
  _ = await==two-await
  // expected-warning@-1 2 {{future versions of Swift reserve the word 'await'; if this name is unavoidable, use backticks to escape it}}

  takesInout(await: &await)
}

func takesUnitFunc(_ f : () -> Void) {}

func takesInout(await : inout Int) {
  await += 1
  // expected-warning@-1 {{future versions of Swift reserve the word 'await'; if this name is unavoidable, use backticks to escape it}}
}

class Klass {
  init() { await() }
  // expected-warning@-1 {{future versions of Swift reserve the word 'await'; if this name is unavoidable, use backticks to escape it}}

  func await() {

    takesUnitFunc(await)
    // expected-warning@-1 {{future versions of Swift reserve the word 'await'; if this name is unavoidable, use backticks to escape it}}
  }

  func method() {
    let _ = self.await
    self.await()

    await()
    // expected-warning@-1 {{future versions of Swift reserve the word 'await'; if this name is unavoidable, use backticks to escape it}}
  }
}