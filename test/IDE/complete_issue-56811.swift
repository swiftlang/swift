// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=COMPLETE | %FileCheck %s

// https://github.com/apple/swift/issues/56811

public enum Endpoint {
  case movieDetail


  func path() -> String {
    switch self {
    case .movieDetail:
      let myInt: Int = 2
      return "\(#^COMPLETE^#)"
    }
  }
}
// CHECK: Decl[LocalVar]/Local/TypeRelation[Convertible]: myInt[#Int#];
