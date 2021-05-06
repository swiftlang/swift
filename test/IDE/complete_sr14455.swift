// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=COMPLETE | %FileCheck %s

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
// CHECK: Begin completions
// CHECK: Decl[LocalVar]/Local/TypeRelation[Convertible]: myInt[#Int#];
// CHECK: End completions
