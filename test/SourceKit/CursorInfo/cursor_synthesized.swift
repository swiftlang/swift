struct Synthesized: Hashable {
  let a: Int
  // RUN: %sourcekitd-test -req=cursor -pos=%(line-1):10 %s -- %s -target %target-triple | %FileCheck -check-prefix=INT %s
  // INT: Int
  // INT-NEXT: s:Si
  // INT-NOT: SYNTHESIZED
}

func synthesized(hasher: inout Hasher) {
  let s = Synthesized(a: 1)
  // RUN: %sourcekitd-test -req=cursor -pos=%(line-1):11 %s -- %s -target %target-triple | %FileCheck -check-prefix=INIT %s
  // INIT: Synthesized
  // INIT-NEXT: s:18cursor_synthesized11SynthesizedV
  // INIT-NOT: SYNTHESIZED
  // INIT: SECONDARY SYMBOLS BEGIN
  // INIT: init(a:)
  // INIT-NEXT: s:18cursor_synthesized11SynthesizedV1aACSi_tcfc
  // INIT: SYNTHESIZED
  // INIT: -----
  // INIT: SECONDARY SYMBOLS END

  _ = s.hashValue
  // RUN: %sourcekitd-test -req=cursor -pos=%(line-1):9 %s -- %s -target %target-triple | %FileCheck -check-prefix=HASH_VALUE %s
  // HASH_VALUE: hashValue
  // HASH_VALUE-NEXT: s:18cursor_synthesized11SynthesizedV9hashValueSivp
  // HASH_VALUE: SYNTHESIZED

  _ = s.hash(into: &hasher)
  // RUN: %sourcekitd-test -req=cursor -pos=%(line-1):9 %s -- %s -target %target-triple | %FileCheck -check-prefix=HASH %s
  // HASH: hash(into:)
  // HASH-NEXT: s:18cursor_synthesized11SynthesizedV4hash4intoys6HasherVz_tF
  // HASH: SYNTHESIZED
}
