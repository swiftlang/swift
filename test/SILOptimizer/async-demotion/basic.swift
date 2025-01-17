// RUN: %target-swift-frontend -enable-experimental-async-demotion -target %target-swift-5.1-abi-triple -module-name main -O -emit-sil -primary-file %s | %FileCheck %s --implicit-check-not hop_to_executor

// REQUIRES: swift_in_compiler
// REQUIRES: concurrency

// for now, check for the expected number of hops after the pass:

// CHECK: hop_to_executor
// CHECK: hop_to_executor
// CHECK: hop_to_executor
// CHECK: hop_to_executor

@MainActor func mainActorFn() {}

func TRUE_LEAF() async {}

func ONE() async { await TWO() }
func TWO() async { await THREE() }
func THREE() async {
  await TRUE_LEAF()
  await ONE()
}

func NOT_CANDIDATE() async {
  await mainActorFn()
  await ONE()
}

@inline(never)
public
func fib(_ n: Int) async -> Int {
  if n <= 1 { return n }
  return await fib(n-1) + fib(n-2)
}



@inline(never)
@MainActor 
public 
func leaf() async {}

@MainActor 
public 
func start() async {
  await leaf()

  if await fib(40) != 102334155 {
  fatalError("bug")
}
}


public 
func checkIt(_ t: Task<Int, Never>) async -> Int {
  await t.value
}
