// RUN: %target-swift-ide-test -reconstruct-type -source-filename %s | FileCheck %s -implicit-check-not="reconstruct"

struct GS<T> {
// CHECK: reconstructed decl from usr for 'GS' is 'struct GS<T>'
// FIXME: why do we get this?
// CHECK: reconstructed decl from usr for 'T' is 'struct GS<T>'

  let a: T.Nope
// CHECK: reconstructed decl from usr for 'a' is 'let a: <<error type>>'
  let b: T
// CHECK: reconstructed decl from usr for 'b' is 'let b: T' 
}

let global1: GS
// CHECK: reconstructed decl from usr for 'global1' is 'let global1: <<error type>>'
let global2 = GS().x
// CHECK: reconstructed decl from usr for 'global2' is 'let global2: <<error type>>'
let global3 = GS<Int>(a: 1, b: 2).b
// CHECK: reconstructed decl from usr for 'global3' is 'let global3: <<error type>>'

protocol P {
// FIXME: missing protocol entries?
// CHECK: cannot reconstruct decl from usr for 'P'
  associatedtype T
// CHECK: cannot reconstruct decl from usr for 'T'
  func foo() -> T
// CHECK: cannot reconstruct decl from usr for 'foo'
}
struct SP: P {
// CHECK: reconstructed decl from usr for 'SP' is 'struct SP : P'
  typealias TT = Self.T
// CHECK: reconstructed decl from usr for 'TT' is 'struct SP : P'
}
