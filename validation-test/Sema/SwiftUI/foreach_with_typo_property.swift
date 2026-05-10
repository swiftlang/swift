// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -target %target-cpu-apple-macosx10.15 -swift-version 5
// REQUIRES: objc_interop
// REQUIRES: OS=macosx
//

//FIXME: this illustrates issue with high impact for member lookup
// Reverting change from PR85591 to missing member impact due to
// interference with extra arguments causes this to be lost again

import SwiftUI
 struct ContentView: View {
   var foos: [F]

   var body: some View {
     ForEach(foos) { foo in //expected-error{{cannot convert value of type '[F]' to expected argument type 'Binding<C>'}} expected-error {{generic parameter 'C' could not be inferred}}
       let name = foo.bat //FIXME: {{value of type 'F' has no member 'bat'}}
     }
   }
 }

 struct F: Identifiable, Hashable {
   var id: String { bar }
   var bar: String
 }
