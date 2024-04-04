// RUN: %target-swift-frontend -emit-ir -parse-as-library -module-name a -g %s -o - | %FileCheck %s
public struct S {
  public func f() {}
}

// CHECK-DAG: distinct !DISubprogram(name: "f", linkageName: "$s1a1SV1fyyF", scope: ![[STRUCT:[0-9]+]], {{.*}}declaration: ![[SDECL:[0-9]+]]
// CHECK-DAG: ![[SDECL]] = !DISubprogram(name: "f", linkageName: "$s1a1SV1fyyF", scope: ![[STRUCT]]
// CHECK-DAG: ![[STRUCT]] = !DICompositeType(tag: DW_TAG_structure_type, name: "S"

public class C {
  public func f() {}
}

// CHECK-DAG: distinct !DISubprogram(name: "f", linkageName: "$s1a1CC1fyyF", scope: ![[CLASS:[0-9]+]], {{.*}}declaration: ![[CDECL:[0-9]+]]
// CHECK-DAG: ![[CDECL]] = !DISubprogram(name: "f", linkageName: "$s1a1CC1fyyF", scope: ![[CLASS]]
// CHECK-DAG: ![[CLASS]] = !DICompositeType(tag: DW_TAG_structure_type, name: "C"
