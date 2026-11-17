// {"kind":"complete","original":"b9b0dab2","signature":"verifyRedundantConformances"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol b<c> {
  associatedtype c
  associatedtype d: e where d.a == Self
  >
}
protocol e {
  associatedtype a: b
  associatedtype f: g where f.h == Self
}
protocol g: Identifiable where ID == h.a.c {
  associatedtype h: e
}
#^^#
