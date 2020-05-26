// RUN: %target-typecheck-verify-swift -target x86_64-apple-macosx10.15 -swift-version 5
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI
import Combine

final class MyObservableObject: ObservableObject {
    @Published private(set) var isDoingTheThing = false
}

struct MyView: View {
    @EnvironmentObject var observableObject: MyObservableObject
            
    var body: some View {
        MyBindingView(doTheThing: $observableObject.isDoingTheThing) // expected-error{{cannot assign to property: 'isDoingTheThing' setter is inaccessible}}
    }
}

struct MyBindingView: View {
    @Binding var doTheThing: Bool
    
    var body: some View {
        Text(doTheThing ? "Doing The Thing" : "Doing Sweet Nothing")
    }
}
