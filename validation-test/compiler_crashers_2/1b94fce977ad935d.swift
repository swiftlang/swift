// {"kind":"typecheck","signature":"printDifferentiableAttrArguments(swift::DifferentiableAttr const*, swift::ASTPrinter&, swift::PrintOptions const&, swift::Decl const*, bool)","signatureAssert":"Assertion failed: (original && \"Must resolve original declaration\"), function printDifferentiableAttrArguments"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a(@differentiable _ =
