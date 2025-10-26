// {"signature":"(anonymous namespace)::Verifier::verifyChecked(swift::VarDecl*)"}
// RUN: not --crash %target-swift-frontend -emit-sil %s

// https://github.com/swiftlang/swift/issues/80657
enum E {
  case e

  static func foo() {
    _ = { [self] in
      switch e {
      case e:
        break
      default:
        break
      }
    }
  }
}
