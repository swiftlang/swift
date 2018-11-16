// RUN: %target-swift-frontend -emit-ir -module-name test %S/Inputs/protocol-extension-init-helper.swift -primary-file %s

// SE-9233: compute layout when emitting an other-constructor reference
extension P {
  public init(possibly: Bool) {
    self.init()
  }
}
