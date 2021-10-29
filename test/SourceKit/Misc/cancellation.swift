// Check that we can cancel requests.
// We need to wait a little bit after request scheduling and cancellation to make sure we are not cancelling the request before it got scheduled.

// RUN: not %sourcekitd-test -req=cursor -id=slow -async -pos=9:5 -simulate-long-request=5000 %s -- %s == \
// RUN: -shell -- sleep 1 == \
// RUN: -cancel=slow 2>&1 \
// RUN: | %FileCheck %s

let x = "Hello World"

// CHECK: error response (Request Cancelled)
