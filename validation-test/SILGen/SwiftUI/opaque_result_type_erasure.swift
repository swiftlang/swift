// RUN: %target-swift-frontend -emit-silgen %s -target %target-cpu-apple-macosx11 -swift-version 5

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

struct MyView: View {
    @State private var isPresented = false

    var body: some View {
        NavigationView {
            Form {
                subsection
            }
            .listStyle(DefaultListStyle())
            .navigationTitle(Text(""))
            .sheet(isPresented: $isPresented,
                   onDismiss: { },
                   content: { Text("") })
        }
    }

    private var subsection: some View {
        Text("")
    }
}
