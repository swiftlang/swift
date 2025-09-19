// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::setType(swift::ASTNode, swift::Type)","signatureAssert":"Assertion failed: (!node.isNull() && \"Cannot set type information on null node\"), function setType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a let() = {(@a)in
