// {"kind":"complete","original":"4249eb4f","signature":"swift::DependentMemberType::get(swift::Type, swift::AssociatedTypeDecl*)"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a { associatedtype b }
struct c func d<e: Collection>(e, e.Element.b ) where e.Element: a func bar(arr: c) { d(arr) #^^#
