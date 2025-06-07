// RUN: %target-swift-frontend -primary-file %s -target %target-swift-5.1-abi-triple -c -o /dev/null -O -disable-llvm-optzns

// REQUIRES: concurrency

// Check that -disable-llvm-optzns does not crash the compiler

func testit() async {
  print(1)
}
