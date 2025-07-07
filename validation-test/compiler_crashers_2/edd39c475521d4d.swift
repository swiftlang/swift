// {"signature":"swift::ForeignRepresentationInfo::isRepresentableAsOptional() const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
a!!= 1
