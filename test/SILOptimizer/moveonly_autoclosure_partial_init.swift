// RUN: %target-swift-frontend -emit-sil -verify -sil-verify-all -O %s
// rdar://191300441

struct Box: ~Copyable {
  var a = UniqueArray(repeating: 0, count: 4)
}

struct S: ~Copyable {
  var flag = false
  var x: Int
  var y: Int
  var box: Box
  
  init(bad n: Int) {
    x = n
    y = n
    flag = n < 0 || x < 20
    box = Box()  // ~Copyable property initialized AFTER autoclosure
  }
  
  deinit {}  // needed to reproduce with the 2026-10-17 snapshot
}

// print(S(okay: 1).x)
print(S(bad: 1).x)
