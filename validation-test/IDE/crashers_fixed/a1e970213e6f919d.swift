// {"kind":"complete","original":"158bd92d","signature":"swift::TypeTransform<swift::Type::transformRec(llvm::function_ref<std::__1::optional<swift::Type> (swift::TypeBase*)>) const::Transform>::doIt(swift::Type, swift::TypePosition)"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a<b > : c protocol d { associatedtype b }
protocol c: d where b: e protocol f { associatedtype g: d associatedtype b}
protocol h: f where g == a<b> protocol i : f protocol e { associatedtype j: i
#^^#
associatedtype k: h where k.b == j.b
