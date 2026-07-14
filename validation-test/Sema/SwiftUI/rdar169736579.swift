// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx12 -verify-ignore-unrelated -solver-disable-enumerate-supertypes

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import Foundation
import SwiftUI

class MyClass: NSObject, Identifiable {
    let dataType = "test"
}

struct ContentView: View {
    let array = [MyClass]()
    let value = 5
    var body: some View {
        ForEach(array) { element in
            switch element.dataType {
            case "test":
                ChildView(element: element, extraArg: value)
                // expected-error@-1 {{extra argument 'extraArg' in call}}
            default:
                Text("unrecognized value")
            }
        }
    }
}

struct ChildView: View {
    let element: MyClass

    var body: some View {
        Text("...")
    }
}
