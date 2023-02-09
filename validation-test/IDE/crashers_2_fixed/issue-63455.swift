// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token COMPLETE

func sheet(onDismiss: () -> Void) -> EmptyView {
  fatalError()
}

@resultBuilder struct ViewBuilder2 {
  static func buildBlock(_ content: EmptyView) -> EmptyView {
    return content
  }
}

struct EmptyView {}

struct SettingsView {
  var importedFile: Int?

  @ViewBuilder2 var body2: EmptyView {
    sheet {
      #^COMPLETE^#if let url = self.importedFile {
        print(url)
      }
    }
  }
}
