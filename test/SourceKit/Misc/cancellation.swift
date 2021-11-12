// Check that we can cancel requests.

// RUN: not %sourcekitd-test -req=cursor -id=slow -async -pos=7:5 -simulate-long-request=5000 %s -- %s == \
// RUN: -cancel=slow 2>&1 \
// RUN: | %FileCheck %s

let x = "Hello World"

// CHECK: error response (Request Cancelled)
