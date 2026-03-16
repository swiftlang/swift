// RUN: not --crash %target-typecheck-verify-swift -target %target-cpu-apple-macosx11 -swift-version 5 -solver-enable-crash-on-valid-salvage
// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx13 -swift-version 5 -solver-enable-crash-on-valid-salvage
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// FIXME: The first expression ends up in salvage with the old deployment
// target.

import SwiftUI

struct Folder: Identifiable {
  var id: String
  var name: String
}

struct FolderView: View {
  let folderList: [Folder]
  var currentFolder: String

  var body: some View {
    ForEach(folderList) { folder in
      Text(folder.name).foregroundStyle(
        folder.id == currentFolder ? .blue : .secondary)
    }
  }
}

struct MyButtonStyle: PrimitiveButtonStyle {
  @Environment(\.isEnabled)
  private var isEnabled

  func makeBody(configuration: Configuration) -> some View {
    Button(action: configuration.trigger) {
      configuration.label.foregroundStyle(isEnabled ? .white : .secondary)
    }
    .background {
      return Color.clear
    }
  }
}

enum Barn<Animal> {
  case animal(Animal)
}

struct Horse {}

struct Hay {}

func emptyPlaceholder(_ text: Text, _ flag: Bool, _ color: Color,
                      _ barn: Barn<(hay: Hay, horses: [Horse])>) -> some View {
  return AnyView(
    VStack() {
      text.foregroundStyle(flag ? .secondary : color)

      if case let .animal((hay, _)) = barn {  // expected-warning {{immutable value 'hay' was never used; consider replacing with '_' or removing it}}
        EmptyView()
      } else {
        EmptyView()
      }
    }
  )
}
