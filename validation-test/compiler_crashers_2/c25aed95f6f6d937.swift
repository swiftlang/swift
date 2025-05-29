// {"signature":"Assertion failed: (detail::isPresent(Val) && \"dyn_cast on a non-existent value\"), function dyn_cast, file Casting.h, line 662."}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  extension {
    a {
      func b {
        super
