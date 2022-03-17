// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token COMPLETE

func foo() {
  bar {
    .dragging(arg1: #^COMPLETE^#, arg2: drag ?? .zero)
  }
}
