// RUN: not %target-swift-frontend %s -typecheck
// REQUIRES: OS=macosx

let array = [Dictionary]()
