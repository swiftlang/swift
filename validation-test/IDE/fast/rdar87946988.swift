// RUN: %batch-code-completion -solver-scope-threshold=500 -code-completion-verify-usr-to-decl=false
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

class Obj: ObservableObject {
  @Published var xs: [[String]] = []
}

extension ForEach {
  func foo() -> some View {}
}

struct V: View {
  @ObservedObject var obj: Obj

  var body: some View {
    ForEach(Array(obj.xs.enumerated()), id: \.0) { i, strs in
      ForEach(Array(strs.enumerated()), id: \.0) { j, str in
        Text(str)
          .padding()
          .frame(width: 0)
          .background(.red)
      }
    }
    .#^COMPLETE^#
    // COMPLETE: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: foo()[#View#]; name=foo()
  }
}
