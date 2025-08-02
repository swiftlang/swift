// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::repairFailures(swift::Type, swift::Type, swift::constraints::ConstraintKind, swift::optionset::OptionSet<swift::constraints::ConstraintSystem::TypeMatchFlags, unsigned int>, llvm::SmallVectorImpl<swift::constraints::RestrictionOrFix>&, swift::constraints::ConstraintLocatorBuilder)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Foundation func a(b: AnyClass?) { b (b
