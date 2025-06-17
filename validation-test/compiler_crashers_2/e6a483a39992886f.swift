// {"signature":"swift::BuiltinTupleDecl::getTupleSelfType(swift::ExtensionDecl const*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
typealias a<b, c> = () extension a
