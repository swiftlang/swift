// {"kind":"complete","original":"3d8ae2c1","signature":"checkCompletionResult","useSourceOrderCompletion":true}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-order-completion -source-filename %s
protocol a { associatedtype b
#^^#
associatedtype c: a where c.b == Self, c .c == b.b }
extension Array: a where Element: a, Element.b == Array<Element.c> { typealias c #^d^#=
