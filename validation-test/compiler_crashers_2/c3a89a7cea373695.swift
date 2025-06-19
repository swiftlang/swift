// {"signature":"swift::constraints::doesMemberRefApplyCurriedSelf(swift::Type, swift::ValueDecl const*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
Swift < .Int
