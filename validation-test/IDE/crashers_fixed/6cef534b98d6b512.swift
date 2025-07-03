// {"kind":"complete","signature":"swift::constraints::Solution::simplifyType(swift::Type, bool) const"}
// RUN: %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -code-completion-diagnostics -source-filename %s
for (a(b, d)) [
#^COMPLETE^#, 0, (repeat.c)].enumerated(
