// RUN: %target-typecheck-verify-swift

class A {
  static var a: Int = 0
  static var b: Int = 42

  func foo(_ ptr: UnsafeMutableRawPointer?) {
    switch ptr {
      case (&A.a)?: break
      case (&A.b)?: break
      default: break
    }
  }

  func bar(_ ptr: UnsafeRawPointer) {
    switch ptr {
      case &A.a: break
      case &A.b: break
      default: break
    }
  }
}
