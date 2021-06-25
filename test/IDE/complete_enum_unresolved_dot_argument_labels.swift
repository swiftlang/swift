// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COMPLETE | %FileCheck %s

enum DragState {
  case inactive
  case dragging(translation: Int)
}

func foo() {
  var state = DragState.inactive
  state = .dragging(#^COMPLETE^#
}

// CHECK: Begin completions, 1 item
// CHECK: Pattern/CurrModule/Flair[ArgLabels]/TypeRelation[Identical]: ['(']{#translation: Int#}[')'][#DragState#];
// CHECK: End completions
