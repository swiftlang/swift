// {"signature":"swift::ForeignRepresentationInfo::isRepresentableAsOptional() const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: asserts
a!!= 1
