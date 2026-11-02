// RUN: %target-swift-frontend -emit-sil -verify -Onone %s -o /dev/null
// RUN: %target-swift-frontend -emit-sil -verify -O %s -o /dev/null

// REQUIRES: rdar191070944
//
// See rdar://191070944 ([exclusivity] DiagnoseStaticExclusivity rejects a conditional conflict on a noncopyable
// variable captured by a local function)
//
// FIXME: MandatoryAllocBoxToStack always promotes the box of a noncopyable
// variable, because the move-only checker runs after it and requires it. After
// promotion, DiagnoseStaticExclusivity treats the capture as a non-escaping
// capture and rejects a conflict that only happens on some paths. Dynamic
// enforcement would accept this program whenever `flag` is false. Fix this by
// running the new move checker before MandatoryAllocBoxToStack, so that
// promotion isn't mandatory for noncopyable variables. rdar://179406916

struct NC: ~Copyable {
  var i = 0
}

func test(_ flag: Bool) {
  var x = NC()
  func update() {
    if flag {
      x.i += 1
    }
  }
  func apply(_ value: inout NC) {
    update()
  }
  apply(&x) // no error expected
}
