// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

//--- Color.swift
extension Invalid {}

//--- MovieRow.swift
struct Bar {}

fileprivate let bar: Bar = {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):7 %t/MovieRow.swift -- %t/MovieRow.swift %t/Color.swift | %FileCheck %s
  let bar = Bar()
  // CHECK: source.lang.swift.decl.var.local ([[@LINE-1]]:7-[[@LINE-1]]:10)
  // CHECK-NEXT: bar
  // CHECK: RELATED BEGIN
  // CHECK-NEXT: <RelatedName usr="s:4main3bar33_F48676AE0C86F007C79C860E40EDA2D3LLAA3BarVvp">bar</RelatedName>
  // CHECK-NEXT: RELATED END
  return bar
}()
