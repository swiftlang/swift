// {"signature":"swift::ExistentialArchetypeType::get(swift::CanType)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a where b == Self
  protocol c : a where Self
  : CustomStringConvertible
    var d e c.description
