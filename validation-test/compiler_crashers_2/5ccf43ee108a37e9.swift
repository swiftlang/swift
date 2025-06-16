// {"signature":"swift::GenericEnvironment::mapTypeIntoContext(swift::Type) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@abi(func a->b) func a < b
