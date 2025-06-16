// {"signature":"deriveBodyRawRepresentable_init(swift::AbstractFunctionDecl*, void*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
a enum a : Int { case = #/
