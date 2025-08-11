// {"signature":"swift::TypeBase::computeCanonicalType()"}
// RUN: not %target-swift-frontend -typecheck %s
func a (Int -> Int = { $0 func b( () = {}? ) func b {
    / 1
