// {"kind":"typecheck","signature":"swift::AccessLevelRequest::cacheResult(swift::AccessLevel) const","signatureAssert":"Assertion failed: (!hasAccess() && \"access already set\"), function setAccess"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a < b extension a where c == d {
  protocol d
