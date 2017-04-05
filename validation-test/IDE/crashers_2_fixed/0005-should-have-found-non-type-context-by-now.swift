// RUN: %target-swift-ide-test -code-completion -code-completion-token=A1 -source-filename=%s
// RUN: %target-swift-ide-test -code-completion -code-completion-token=B1 -source-filename=%s
// RUN: %target-swift-ide-test -code-completion -code-completion-token=C1 -source-filename=%s

// RUN: %target-swift-ide-test -code-completion -code-completion-token=A2 -source-filename=%s
// RUN: %target-swift-ide-test -code-completion -code-completion-token=B2 -source-filename=%s
// RUN: %target-swift-ide-test -code-completion -code-completion-token=C2 -source-filename=%s

// RUN: %target-swift-ide-test -code-completion -code-completion-token=A3 -source-filename=%s
// RUN: %target-swift-ide-test -code-completion -code-completion-token=B3 -source-filename=%s

class a1<b#^A1^#> {}
struct a2<b#^B1^#> {}
enum a3<b#^C1^#> {}

class a4<b> where c == b#^A2^# {}
struct a5<b> where c == b#^B2^# {}
enum a6<b> where c == b#^C2^# {}

func f1<b#^A3^#> {}
func f2<b>() where c == b#^B3^# {}
