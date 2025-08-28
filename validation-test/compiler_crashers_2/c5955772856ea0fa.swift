// {"kind":"typecheck","signature":"swift::constraints::Constraint::create(swift::constraints::ConstraintSystem&, swift::constraints::ConstraintKind, swift::Type, swift::Type, swift::constraints::ConstraintLocator*, llvm::ArrayRef<swift::TypeVariableType*>)","signatureAssert":"Assertion failed: ((kind != ConstraintKind::ConformsTo && kind != ConstraintKind::NonisolatedConformsTo && kind != ConstraintKind::TransitivelyConformsTo) || second->isExistentialType()), function create"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a let : a.Type.Type = b->c
