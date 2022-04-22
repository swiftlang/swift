// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

struct AStruct {
  let aField: MyEnum = .aCase // expected-note {{change 'let' to 'var' to make it mutable}}
}

enum MyEnum {
case aCase
}

extension EmptyView {
  func doImport(showImport: Binding<Bool>, anEnum: MyEnum) -> some View {
    EmptyView()
  }
}

struct SegFaultingView: View {
  @Binding var aStruct: AStruct
  @Binding var showImport: Bool
  @State var importMessage: String = "none"

  var body: some View {
    EmptyView()
      .doImport(showImport: showImport, // expected-error {{cannot convert value 'showImport' of type 'Bool' to expected type 'Binding<Bool>', use wrapper instead}}
                importMessage: importMessage, // expected-error {{extra argument 'importMessage' in call}}
                anEnum: $aStruct.aField) // expected-error {{cannot convert value of type 'Binding<MyEnum>' to expected argument type 'MyEnum'}}
                // expected-error@-1 {{cannot assign to property: 'aField' is a 'let' constant}}
  }
}
