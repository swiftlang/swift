// {"kind":"typecheck","signature":"(anonymous namespace)::DeclChecker::visit(swift::Decl*)","signatureAssert":"Assertion failed: (false && \"Huh?\"), function isValidExtendedTypeForTupleExtension"}
// RUN: not %target-swift-frontend -typecheck %s
extension repeat (
