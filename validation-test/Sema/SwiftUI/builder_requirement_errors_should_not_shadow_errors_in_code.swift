// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

extension [UInt8] {
  var toASCII : String? { nil }
}

// rdar://111120803
do {
  struct MissingOptionalUnwrap : View {
    var data: [UInt8]

    var body: some View {
      Group {
        Text("")
        Text(data.toASCII)
        // expected-error@-1 {{value of optional type 'String?' must be unwrapped to a value of type 'String'}}
        // expected-note@-2 {{coalesce using '??' to provide a default when the optional value contains 'nil'}}
        // expected-note@-3 {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}}
      }
    }
  }
}

// rdar://112928810
do {
  struct DismissAction {
    func callAsFunction() {
    }
  }

  struct TestConcurrencyMismatch : View {
    private var dismiss : DismissAction

    var body: some View {
      List {
      }
      .toolbar {
        ToolbarItem(placement: .cancellationAction) {
          Button("Dismiss") {
            dismiss()
          }
        }
        ToolbarItem(placement: .primaryAction) {
          Button("Done") {
            MainActor.run {
              // expected-error@-1 {{cannot pass function of type '@Sendable () async -> ()' to parameter expecting synchronous function type}}
              await dismiss()
              // expected-note@-1 {{'async' inferred from asynchronous operation used here}}
            }
          }
        }
      }
    }
  }
}

// rdar://115784749
do {
  struct InvalidArgumentPlacement : View {
    var body: some View {
      let base = RoundedRectangle(cornerRadius: 12)
      Group {
        base.strokeBorder(lineWidth: 2, LinearGradient(gradient: Gradient(colors: [.red, .blue]), startPoint: .top, endPoint: .bottom))
        // expected-error@-1 {{unnamed argument #2 must precede argument 'lineWidth'}}
        Text("test")
          .font(.system(size: 200))
          .minimumScaleFactor(0.01)
          .aspectRatio(1, contentMode: .fit)
      }
    }
  }
}

// rdar://118326796
do {
  class Model: ObservableObject {
  }

  struct InvalidRef: View {
    @State private var isLoading = true

    var body: some View {
      Group {
        if isLoading{
          EmptyView().onAppear {
            DispatchQueue.main.asyncAfter(deadline: .now() + 2) {
              isLoading = false
            }
          }
        } else {
          NavigationView {
          }
          .environmentObject(Model)
          // expected-error@-1 {{type 'Model.Type' cannot conform to 'ObservableObject'}}
          // expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}
          // expected-note@-3 {{required by instance method 'environmentObject' where 'T' = 'Model.Type'}}
        }
      }
    }
  }
}

// rdar://106804509
do {
  struct Entry: Identifiable {
    var id: String { name }
    let name: String
    let type: String = "string"
  }

  struct MissingLabelTest: View {
    var entry: [Entry] = []

    var body: some View {
      VStack(alignment: .leading) {
        ForEach(entry) { entry in
          HStack {
            Text(entry.name)
            Text("(\(entry.type))").foregroundColor(.secondary)
          }
          ViewWithTitle("title")
          // expected-error@-1 {{missing argument label 'title:' in call}}
        }
      }
    }
  }

  struct ViewWithTitle: View {
    let title: String

    var body: some View {
      Text(title)
    }
  }
}

// rdar://118374670
do {
  struct MissingWrapperInnerView: View {
    @State var cond = false

    var body: some View {
      Group {
        Group {
          EmptyView()
        }
        .disabled(true)
        Toggle(isOn: cond) {
          // expected-error@-1 {{cannot convert value 'cond' of type 'Bool' to expected type 'Binding<Bool>', use wrapper instead}} {{22-22=$}}
          Text("")
        }
      }
      .disabled(true)
    }
  }
}
