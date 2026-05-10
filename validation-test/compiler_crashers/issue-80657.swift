// {"issueID":80657,"kind":"emit-sil","signature":"(anonymous namespace)::Verifier::verifyChecked(swift::VarDecl*)","signatureNext":"Verifier::walkToDeclPost"}
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
