// {"kind":"typecheck","signature":"swift::Decl::attachParsedAttrs(swift::DeclAttributes)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@differentiable () let a, b
