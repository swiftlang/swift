// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: OS=macosx

let array = [Dictionary]()
