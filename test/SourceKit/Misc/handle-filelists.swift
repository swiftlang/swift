let x = 10
x.

// RUN: %sourcekitd-test -req=complete -pos=2:3 %s -- -driver-filelist-threshold=0 %s | %FileCheck %s -check-prefix=COMPLETE
// COMPLETE: littleEndian
