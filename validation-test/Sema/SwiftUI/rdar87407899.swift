// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

// Stand-in for SwiftUI's `@State`, which became a macro in recent SDKs and would
// otherwise require the SwiftUIMacros plugin. This test exercises type-checking
// diagnostics, not `@State` itself; the box reproduces `@State`'s nonmutating
// setter and `Binding` projected value.
@propertyWrapper
struct FakeState<Value> {
  final class Box { var value: Value; init(_ value: Value) { self.value = value } }
  private let box: Box
  init(wrappedValue: Value) { box = Box(wrappedValue) }
  var wrappedValue: Value {
    get { box.value }
    nonmutating set { box.value = newValue }
  }
  var projectedValue: Binding<Value> {
    Binding(get: { box.value }, set: { box.value = $0 })
  }
}

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
  @FakeState var importMessage: String = "none"

  var body: some View {
    EmptyView()
      .doImport(showImport: showImport, // expected-error {{cannot convert value 'showImport' of type 'Bool' to expected type 'Binding<Bool>', use wrapper instead}}
                importMessage: importMessage, // expected-error {{extra argument 'importMessage' in call}}
                anEnum: $aStruct.aField) // expected-error {{cannot convert value of type 'Binding<MyEnum>' to expected argument type 'MyEnum'}}
                // expected-error@-1 {{cannot assign to property: 'aField' is a 'let' constant}}
  }
}
