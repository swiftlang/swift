// {"kind":"typecheck","original":"2e6c789c","signature":"swift::DeclAndTypePrinter::Implementation::visitVarDecl(swift::VarDecl*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
a
@TaskLocal init()
