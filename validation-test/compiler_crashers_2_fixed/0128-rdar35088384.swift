// RUN: %target-swift-frontend -typecheck -verify %s

protocol Command {}

struct A : Command {}
struct B : Command {}

// This used to crash in Xcode 9 GM, and fails with a diagnostic in more
// recent swift-4.0-branch builds, because we incorrectly infer the type
// of the array literal as [Any].

let a = Array<Command.Type>([A.self, B.self])
