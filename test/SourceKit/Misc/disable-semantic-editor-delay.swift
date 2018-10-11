// RUN: not %sourcekitd-test -req=crash 2>&1 | %FileCheck %s -check-prefix=ENABLED
// RUN: SOURCEKIT_DISABLE_SEMA_EDITOR_DELAY=1 not %sourcekitd-test -req=crash 2>&1 | %FileCheck %s -check-prefix=DISABLED

// ENABLED: disabling semantic editor for
// DISABLED-NOT: disabling semantic editor for
