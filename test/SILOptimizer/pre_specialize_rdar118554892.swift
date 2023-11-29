// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-stdlib -O -Xllvm -sil-disable-pass=function-signature-opts -emit-sil %s | %FileCheck %s

import Swift

public struct Foo {}

@_specializeExtension
extension Array {

  @_specialize(exported: true,
               target: _endMutation(),
               where Element == Foo)
  @usableFromInline
  mutating func rdar118554892__specialize_class__endMutation(){ Builtin.unreachable() }
}

extension Sequence where Element: StringProtocol {
  // CHECK-DAG: sil shared @$sST28pre_specialize_rdar118554892Sy7ElementRpzrlE3xxxSSyFSaySSG_Tg5 : $@convention(method) (@guaranteed Array<String>)
  // CHECK-DAG: sil shared @$sST28pre_specialize_rdar118554892Sy7ElementRpzrlE3xxxSSyFSaySsG_Tg5 : $@convention(method) (@guaranteed Array<Substring>)
  @_specialize(where Self == Array<Substring>)
  @_specialize(where Self == Array<String>)
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
