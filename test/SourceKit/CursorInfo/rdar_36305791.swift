public class Observable<Element> {
  public typealias E = Element
}
Observable.create { }

// Check that cursor info on create doesn't crash
// RUN: %sourcekitd-test -req=cursor -cursor-action -pos=4:12 -length 10 %s -- %s | %FileCheck %s
// CHECK: ACTIONS BEGIN
// CHECK: ACTIONS END
