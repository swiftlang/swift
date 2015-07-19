// RUN: not %target-swift-frontend %s -parse

_ = {
  protocol P {
    typealias A
  }
}
