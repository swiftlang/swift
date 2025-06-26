// {"signature":"swift::constraints::ConstraintSystem::setType(swift::ASTNode, swift::Type)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a let() = {(@a)in
