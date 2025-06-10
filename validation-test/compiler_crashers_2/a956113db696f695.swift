// {"signature":"swift::constraints::ConstraintSystem::associateArgumentList(swift::constraints::ConstraintLocator*, swift::ArgumentList*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol b let b = {          (a : b)in switch a {                    case  .c(d(
