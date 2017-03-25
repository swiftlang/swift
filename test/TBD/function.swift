// RUN: %target-swift-frontend -c -parse-as-library -module-name test -validate-tbd-against-ir %s

public func publicNoArgs() {}
public func publicSomeArgs(_: Int, x: Int) {}

internal func internalNoArgs() {}
internal func internalSomeArgs(_: Int, x: Int) {}

private func privateNoArgs() {}
private func privateSomeArgs(_: Int, x: Int) {}
