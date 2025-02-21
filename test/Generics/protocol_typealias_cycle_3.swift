// RUN: %target-swift-frontend -typecheck %s

protocol P {
  typealias MyFunction =
      @convention(c) (UnsafeMutableRawPointer?,
                      UnsafeMutableRawPointer?,
                      UnsafeMutableRawPointer?) -> CInt
}

// UNSUPPORTED: OS=windows-msvc
