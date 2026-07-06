// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx14 -swift-version 5 -solver-scope-threshold=100000
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// Invalid expressions

import SwiftUI

struct SubView: View {
    var body: some View {
        EmptyView()
    }
}

struct TestView: View {
    @State private var value: (row: Int, column: Int)? = nil
    
    var body: some View {
        Grid {
            ForEach(1...3, id: \.self) { row in
                GridRow {
                    ForEach(1...3, id: \.self) { column in
                        SubView()  // expected-error {{reasonable time}}
                            .onTapGesture {
                                value = (row, column)
                            }
                            // error: you have to unwrap the optional
                            .scaleEffect(value == (row, column) ? 1.5 : 1.0)
                            .foregroundStyle(value == (row, column) ? .red : .tint)
                            .font(.system(size: value == (row, column) ? 50 : 30))
                    }
                }
            }
        }
    }
}
