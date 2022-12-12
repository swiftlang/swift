// RUN: %empty-directory(%t/split)
// RUN: %{python} %utils/split_file.py -o %t/split %s
// RUN: %empty-directory(%t/build)
// RUN: %sourcekitd-test -req=cursor -pos=5:9 %t/split/MovieRow.swift -- %t/split/MovieRow.swift %t/split/Color.swift | %FileCheck %s

// BEGIN Color.swift

extension Invalid {}

// BEGIN MovieRow.swift

struct Bar {}

fileprivate let bar: Bar = {
    let bar = Bar()
    return bar
}()


// CHECK: source.lang.swift.decl.var.local (5:9-5:12)
// CHECK-NEXT: bar
// CHECK: RELATED BEGIN
// CHECK-NEXT: <RelatedName usr="s:4main3bar33_F48676AE0C86F007C79C860E40EDA2D3LLAA3BarVvp">bar</RelatedName>
// CHECK-NEXT: RELATED END
