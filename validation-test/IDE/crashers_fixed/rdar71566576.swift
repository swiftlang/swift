// RUN: %swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename %s

if let item = ["a"].first(where: { #^COMPLETE^# }) {}
