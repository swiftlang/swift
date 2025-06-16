// {"signature":"swift::GenericContext::getGenericParams() const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
typealias a = () extension a : Comparable
