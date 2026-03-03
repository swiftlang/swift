// RUN: %target-swift-frontend -emit-ir %s -target %target-swift-5.9-abi-triple
// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos
// rdar://163379698

// This used to trigger an infinite loop in conformance substitution
// when emitting the opaque type descriptor in IRGen.

// rdar://160804717

import SwiftUI

public struct CategorySplitView<each Destination: View>: View {
    let titleKey: LocalizedStringKey
    let groups: (repeat each Destination)

    public var body: some View {
        TupleView((repeat CategoryGroupSidebar(group: each groups))).navigationTitle(titleKey)
    }
}

public struct CategoryGroupSidebar<Destination>: View {
    let group: Destination

    public var body: some View {
        Text("Hi")
    }
}

