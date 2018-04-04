public class Observable<Element> {
  public typealias E = Element
}
Observable.create { }

// RUN: %sourcekitd-test -req=cursor -cursor-action -pos=4:12 -length 10 %s -- %s | %FileCheck %s

// CHECK: source.lang.swift.ref.module ()
