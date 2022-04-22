// RUN: %target-swift-frontend -typecheck %s -requirement-machine-protocol-signatures=on

protocol P {
  typealias MyFunction =
      @convention(c) (UnsafeMutableRawPointer?,
                      UnsafeMutableRawPointer?,
                      UnsafeMutableRawPointer?) -> CInt
}

