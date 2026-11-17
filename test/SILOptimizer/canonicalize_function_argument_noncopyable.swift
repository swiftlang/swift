// RUN: %target-swift-frontend -emit-sil -O -parse-as-library -module-name test -sil-verify-all %s | %FileCheck %s

// https://github.com/swiftlang/swift/issues/93409
// OSSA function-argument canonicalization in SILCombine left a leaked copy when
// a copy of the argument was forwarded into a non-copyable struct which is
// consumed by another non-copyable aggregate.

public final class Instance {}

struct Files: ~Copyable {
  let owner: Instance
}

struct Permit: ~Copyable {
  let instance: Instance
  let tag: Int
}

public struct Context: ~Copyable {
  let files: Files
  let permit: Permit

  init(owner: Instance) {
    files = Files(owner: owner)
    permit = Permit(instance: owner, tag: 1)
  }
}

// CHECK-LABEL: sil @$s4test4makeySiAA8InstanceC_SiAA7ContextVXEtF :
// CHECK:         apply
// CHECK:       } // end sil function '$s4test4makeySiAA8InstanceC_SiAA7ContextVXEtF'
public func make(_ owner: Instance, _ body: (borrowing Context) -> Int) -> Int {
  body(Context(owner: owner))
}
