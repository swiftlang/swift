// {"signature":"swift::AvailabilityScope::verify(swift::AvailabilityScope const*, swift::ASTContext&) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
if
#available({}
  else {
    var a
