// RUN: %target-swift-frontend -module-name test -O -emit-sil -sil-verify-all %s -o /dev/null

// rdar://190471670: When the move-only checker extracted the field switched
// over by `frame!`, it ended the new borrow at the top of the switch_enum's
// successors, before the uses of the guaranteed payload. Lifetime completion
// (which runs because of the `inout` ~Copyable parameter) then inserted an
// extend_lifetime of the borrow, which crashed SemanticARCOpts.

public final class Box {}
public struct Frame: ~Copyable { var b: Box }
public struct Ctx: ~Copyable { var y: Int }

@inline(never) func use(_ b: Box) {}
@inline(__always) func ack(_ f: borrowing Frame) { use(f.b) }

public struct Items: ~Copyable {
  var frame: Frame?

  public func allAcked(_ ctx: inout Ctx) {
    ack(frame!)
  }
}
