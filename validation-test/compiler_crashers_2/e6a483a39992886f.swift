// {"kind":"typecheck","signature":"swift::BuiltinTupleDecl::getTupleSelfType(swift::ExtensionDecl const*) const","signatureAssert":"Assertion failed: (genericParams->getParams().size() == 1), function getTupleSelfType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
typealias a<b, c> = () extension a
