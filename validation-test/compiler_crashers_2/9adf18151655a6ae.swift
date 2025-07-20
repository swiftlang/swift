// {"kind":"typecheck","signature":"(anonymous namespace)::DeclChecker::visit(swift::Decl*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
typealias a<b> = (repeat b)protocol c extension a : c
