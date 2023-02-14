// RUN: %target-run-simple-swift(-enable-experimental-feature SwiftParser -enable-experimental-feature ParserASTGen)

// REQUIRES: executable_test

// -enable-experimental-feature requires and asserts build
// REQUIRES: asserts

func test1(x: Int, fn: (Int) -> Int) -> Int {
  let xx = fn(42)
  return fn(x)
}

func test2(b: Bool) {
  if b {
    print("TRUE")
  } else {
    print("FALSE")
  }

  let x = true
}

func test3(y: Int) -> Int {
  let x = y
  return x
}
