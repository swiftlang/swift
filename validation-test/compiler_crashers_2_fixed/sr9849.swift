// RUN: %target-swift-frontend -Xllvm -sil-verify-after-pass=loadable-address -emit-ir -o %t.ll %s

// Just make sure we don't crash.


struct Large {
  var a: Int = 1
  var b: Int = 1
  var c: Int = 1
  var d: Int = 1
  var e: Int = 1
  var f: Int = 1
  var g: Int = 1
  var h: Int = 1
}

func test(_ x: Large) -> (Large, (Large) -> Large) {
  return (x, {$0})
}
