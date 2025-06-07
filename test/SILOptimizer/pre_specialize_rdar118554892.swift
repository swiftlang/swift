// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-stdlib -O -Xllvm -sil-disable-pass=function-signature-opts -emit-sil %s | %FileCheck %s

// Regression test for rdar://118554892

import Swift

public struct Foo {}

@_specializeExtension
extension Array {
  // The `target:` here triggered the bug we are testing for here. So while we don't test this
  // specialization itself, it has to remain here for the test to properly function.
  @_specialize(exported: true,
               target: _endMutation(),
               where Element == Foo)
  @usableFromInline
  mutating func rdar118554892__specialize_class__endMutation(){ Builtin.unreachable() }
}

extension Sequence where Element: StringProtocol {
  // CHECK-DAG: sil shared [noinline] @$sST28pre_specialize_rdar118554892Sy7ElementRpzrlE3xxxSSyFSaySSG_Tg5 : $@convention(method) (@guaranteed Array<String>)
  // CHECK-DAG: sil shared [noinline] @$sST28pre_specialize_rdar118554892Sy7ElementRpzrlE3xxxSSyFSaySsG_Tg5 : $@convention(method) (@guaranteed Array<Substring>)
  @_specialize(where Self == Array<Substring>)
  @_specialize(where Self == Array<String>)
  @inline(never) // prevent inlining the specialization into the function body
  public func xxx() -> String {
    return _xxx()
  }

  @inline(__always)
  internal func _xxx() -> String {
    var result = ""
    for x in self {
      result.append(x._ephemeralString)
    }
    return result
  }
}
