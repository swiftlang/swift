// A conformance can end up with two SIL witness tables: the original one, and a
// `[specialized]` one created when a generic default implementation in a
// protocol extension had to be specialized for the conforming type. Both mangle
// to the same symbol, so only the specialized one may be emitted. This is only
// observable when witness tables are emitted eagerly rather than lazily, which
// is what `@export(interface)` on the conformance does.

// RUN: %target-swift-frontend %s -module-name main -parse-as-library -enable-experimental-feature Embedded -Osize -emit-sil -o - | %FileCheck -check-prefix SIL %s
// RUN: %target-swift-frontend %s -module-name main -parse-as-library -enable-experimental-feature Embedded -Osize -emit-ir -o - | %FileCheck -check-prefix IR %s

// REQUIRES: swift_feature_Embedded

public protocol P: AnyObject {
  func required()
  func predicate() -> Bool
}

extension P {
  // Specializing this generic default method for the conforming type is what
  // produces the second witness table.
  public func isSame(_ rhs: some P) -> Bool {
    if let rhs = rhs as? Self { return rhs.predicate() }
    return false
  }
  public func predicate() -> Bool { true }
}

public class C {}

@export(interface)
extension C: P {
  public func required() {}
}

// Both witness tables exist in SIL ...
// SIL-DAG: sil_witness_table C: P module main {
// SIL-DAG: sil_witness_table shared [specialized] C: P module main {

// ... but exactly one is emitted, and it is the specialized one: the witness for
// `predicate()` carries a specialization suffix rather than being the generic
// default implementation's thunk.
// IR: @"$e4main1CCAA1PAAWP" ={{.*}} constant [{{[0-9]+}} x ptr] [{{.*}}@"$e4main1CCAA1PA2aDP9predicateSbyFTWAC_Tg{{[a-z]*}}5"{{.*}}]
// IR-NOT: @"$e4main1CCAA1PAAWP" =

var g: any P = C()

@inline(never)
public func check(_ p: any P) -> Bool {
  return p.predicate()
}

public func go() -> Bool { return check(C()) }
