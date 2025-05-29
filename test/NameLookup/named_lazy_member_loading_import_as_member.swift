// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// RUN: %empty-directory(%t/stats-lazy)
// RUN: %empty-directory(%t/stats-eager)

// RUN: %target-swift-frontend -typecheck -I %S/Inputs/NamedLazyMembers %s -stats-output-dir %t/stats-lazy
// RUN: %target-swift-frontend -typecheck -I %S/Inputs/NamedLazyMembers %s -stats-output-dir %t/stats-eager -disable-named-lazy-import-as-member-loading

// stats-lazy should only have imported SimpleDoer.Mode; stats-eager should also
// have imported SimpleDoer.Kind
// RUN: %{python} %utils/process-stats-dir.py --evaluate-delta 'NumTotalClangImportedEntities == 1' %t/stats-lazy %t/stats-eager

import NamedLazyMembers

func fn(_: SimpleDoer.Mode) {}
