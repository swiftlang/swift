// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5
// REQUIRES: objc_interop
// REQUIRES: OS=macosx
//

import SwiftUI
 struct ContentView: View {
   var foos: [F]

   var body: some View {
     ForEach(foos) { foo in
       let name = foo.bat //expected-error{{value of type 'F' has no member 'bat'}}
     }
   }
 }

 struct F: Identifiable, Hashable {
   var id: String { bar }
   var bar: String // expected-note {{'bar' declared here}}
 }
