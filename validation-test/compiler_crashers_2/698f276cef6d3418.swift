// {"signature":"swift::DefaultAndMaxAccessLevelRequest::cacheResult(std::__1::pair<swift::AccessLevel, swift::AccessLevel>) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a < b extension a where b : c, d == a {
  protocol c struct a
