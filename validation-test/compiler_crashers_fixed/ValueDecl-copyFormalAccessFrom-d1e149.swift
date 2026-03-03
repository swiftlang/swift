// RUN: not %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Distributed
distributed open actor a {
}

open distributed actor DA: Comparable {}