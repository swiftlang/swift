// {"signature":"swift::TypeBase::computeCanonicalType()"}
// RUN: env DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
// REQUIRES: target-same-as-host
// REQUIRES: no_asan
func a (Int -> Int = { $0 func b( () = {}? ) func b {
    / 1
