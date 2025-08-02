// {"kind":"typecheck","signature":"printDifferentiableAttrArguments(swift::DifferentiableAttr const*, swift::ASTPrinter&, swift::PrintOptions const&, swift::Decl const*, bool)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a(@differentiable _ =
