// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5 -

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

struct Person {
  var fullName: String
}

enum State<Input, Output, Failure: Error> {
  case success(Input, Output)
  case failure(Input, Failure)
}

// Type-checker finds this overload based on
// `extension Optional : View where Wrapped : View { ... }`
extension View {
  func failure(_: String) -> some View { EmptyView() }
}

struct MyTest : View {
  var state: State<Person, UUID, Error>?

  // `switch` has to be anchored on `AccessorDecl` to reproduce
  // a crash.
  var body: some View {
    switch state {
    case nil:
      Text("nil")
    case .success(let person, _), .failure(let person, _):
      Text(person.fullName)
    }
  }
}
