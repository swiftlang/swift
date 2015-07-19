// RUN: not %target-swift-frontend %s -parse

_ = {
  typealias A = Int
}
