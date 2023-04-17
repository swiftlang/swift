// RUN: %target-swift-frontend -module-name a -parse-as-library -emit-sil -g %s | %FileCheck %s

func sink<T>(_ t: T) {}

public func f(_ xs: [String?]) {
  for x in xs {
    let x = x!
    sink(x)
  }
}
// CHECK: sil_scope [[F:[0-9]+]] { loc "{{.*}}":5:13 parent @$s1a1fyySaySSSgGF
// CHECK: sil_scope [[S0:[0-9]+]] { loc "{{.*}}":6:3 parent [[F]] }
// CHECK: sil_scope [[S1:[0-9]+]] { loc "{{.*}}":6:15 parent [[S0]] }
// CHECK: sil_scope [[S3:[0-9]+]] { loc "{{.*}}":7:9 parent [[S1]] }
// CHECK: sil_scope [[S4:[0-9]+]] { loc "{{.*}}":7:13 parent [[S3]] }

// CHECK: debug_value %[[X:.*]] : $Optional<String>, let, name "x", {{.*}}, scope [[S0]]
// CHECK: retain_value %[[X]] : $Optional<String>, {{.*}}, scope [[S4]]
// CHECK: debug_value %[[X1:[0-9]+]] : $String, let, name "x", {{.*}}, scope [[S3]]
// CHECK:  release_value %[[X1]] : $String, {{.*}}, scope [[S3]]
// CHECK:  release_value %[[X]] : $Optional<String>, {{.*}}, scope [[S3]]
