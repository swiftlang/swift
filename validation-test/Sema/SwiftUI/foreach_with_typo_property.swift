// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -target %target-cpu-apple-macosx10.15 -swift-version 5

// REQUIRES: objc_interop
// REQUIRES: OS=macosx


import SwiftUI

struct ContentView: View {
   var foos: [F]

   var body: some View {
     ForEach(foos) { foo in
       let name = foo.bat // expected-error {{value of type 'F' has no member 'bat'; did you mean 'bar'?}}
     }
   }
 }

struct ExtraContentView: View {
  var foos: [F]
  var body: some View {
    ForEach(foos) { foo in
      switch foo.id {
      case "1":
        ChildView(id: foo.id, extra: 42) // expected-error {{extra argument 'extra' in call}}
      default:
        EmptyView()
      }
    }
  }
}

 struct F: Identifiable, Hashable {
   var id: String { bar }
   var bar: String // expected-note {{'bar' declared here}}
 }

 struct ChildView: View {
   let id: String
   var body: some View { EmptyView() }
 }
