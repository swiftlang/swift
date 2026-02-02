// {"signature":"swift::GenericContext::getGenericParams() const"}
// RUN: not %target-swift-frontend -typecheck %s
typealias a = () extension a : Comparable
