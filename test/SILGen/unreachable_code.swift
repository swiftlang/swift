// RUN: %target-swift-frontend -emit-sil %s -o /dev/null -verify

func testUnreachableAfterReturn() -> Int {
  var x: Int = 3;
  return x;
  x++ //expected-warning {{code after 'return' will never be executed}}
}

func testUnreachableAfterIfReturn(a: Bool) -> Int {
  if a {
    return 1
  } else {
    return 0
  }
  var i: Int = testUnreachableAfterReturn() // expected-warning {{will never be executed}}
}

func testUnreachableForAfterContinue(b: Bool) {
  for (var i:Int = 0; i<10; i++) { 
    var y: Int = 300;
    y++;
    if b {
      break;
    }
    continue;
    y--; // expected-warning {{code after 'continue' will never be executed}}
  }
}

func testUnreachableWhileAfterContinue(b: Bool) {
  var i:Int = 0;
  while (i<10) { 
    var y: Int = 300;
    y++;
    if b {
      break;
    }
    continue;
    i++; // expected-warning {{will never be executed}}
  }
}

func testBreakAndContinue() {
  var i = 0;
  var m = 0;
  for (i = 0; i < 10; ++i) {
    m++
    if m == 15 {
      break
    } else {
      continue
    }
    m++ // expected-warning {{will never be executed}}
  }
}
