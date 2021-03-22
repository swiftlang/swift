// RUN: %sourcekitd-test -req=cursor -pos=5:16 %s -- %s | %FileCheck %s

protocol View {}
struct MyView: View {}
func indicator<T>(_ a: T) -> some View {
  MyView()
}

// CHECK: source.lang.swift.decl.generic_type_param
