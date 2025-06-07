// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx12

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

struct V: View {
    struct M: Identifiable {
        let id: String
    }

    class C: ObservableObject {
        @Published var m: [M]

        init() {
            self.m = []
        }
    }

    @ObservedObject var c: C

    var body: some View {
        Table($c.m) {
            TableColumn("") { $entry in
                Text("hi")
            }
        }
    }
}
