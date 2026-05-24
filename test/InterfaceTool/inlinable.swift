// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift-interface-tool -action minimize-source %t/input.swift > %t/output.swift
// RUN: %diff %t/output.swift %t/expected.swift

//--- input.swift
@inlinable public func inlinableFunc() {
  let x = 1
  print(x)
}

@_transparent public func transparentFunc() {
  print("transparent")
}

@_alwaysEmitIntoClient public func alwaysEmitFunc() {
  print("always")
}

@backDeployed(before: macOS 15.0)
public func backDeployedFunc() {
  print("backdeployed")
}

public func regularFunc() {
  print("regular")
}
//--- expected.swift
@inlinable public func inlinableFunc() {
  let x = 1
  print(x)
}

@_transparent public func transparentFunc() {
  print("transparent")
}

@_alwaysEmitIntoClient public func alwaysEmitFunc() {
  print("always")
}

@backDeployed(before: macOS 15.0)
public func backDeployedFunc() {
  print("backdeployed")
}

public func regularFunc()
