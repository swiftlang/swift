// RUN: %target-swift-frontend %s -target %target-swift-5.9-abi-triple -emit-ir

public enum Enum<T : ~Escapable> : ~Escapable {
  case none
  case some(T)
}

extension Enum: Escapable where T: Escapable {}
