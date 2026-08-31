// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend -typecheck -sdk %t %s
// REQUIRES: OS=macosx
import Distributed
distributed actor a {
}
