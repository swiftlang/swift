@backDeployed(before: SwiftStdlib 6.0)
public func backDeployedFunc() {
  otherFunc()
}

@usableFromInline internal func otherFunc() {}
