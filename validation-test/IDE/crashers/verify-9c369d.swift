// {"kind":"complete","original":"7d066aab","signature":"verify"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype b: a where b.c == Self
  !
  associatedtype c: d where c.e == Self
}
protocol d {
  associatedtype e: a where e.c == Self
}
#^^#
