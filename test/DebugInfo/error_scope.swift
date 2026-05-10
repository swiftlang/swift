// RUN: %target-swift-frontend %s -parse-as-library -g -Xllvm -sil-print-types -emit-sil -o - | %FileCheck %s --check-prefix=CHECK-SIL

public enum Err: Error, Equatable {
  case err
}

public func throwing() throws {
  throw Err.err
}

func sink<T>(_ t: T) {}

public func f() {
  do {
    _ = try throwing()
  } catch let error as Err {
    // CHECK-SIL-DAG: $Err, let, name "error", {{.*}}:[[@LINE-1]]:15, scope [[SCOPE1:[0-9]+]]
    // CHECK-SIL-DAG: sil_scope [[SCOPE1]] {{.*}}:[[@LINE-2]]:5
    sink(error)
  } catch {
    // CHECK-SIL-DAG: $any Error, let, name "error", {{.*}}:[[@LINE-1]]:11, scope [[SCOPE2:[0-9]+]]
    // CHECK-SIL-DAG: sil_scope [[SCOPE2]] {{.*}}:[[@LINE-2]]:11
    sink(error)
  }
}
