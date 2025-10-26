// RUN: not %target-swift-frontend %s -typecheck

_ = {
  protocol P {
    typealias A
  }
}
