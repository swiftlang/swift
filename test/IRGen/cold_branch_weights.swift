// Test the lowering of ColdBlockInfo into LLVM IR via !prof branch weights and `cold` LLVM IR functions.
// Static branch prediction is used here to seed the ColdBlockInfo with data.

// RUN: %target-swift-frontend %s -O \
// RUN:   -enable-throws-prediction \
// RUN:   -module-name=test -emit-irgen \
// RUN:      | tee /Users/kavon/tmp/out.ll \
// RUN:      | %FileCheck --check-prefix CHECK-ENABLED %s

// RUN: %target-swift-frontend %s -O \
// RUN:   -module-name=test -emit-irgen \
// RUN:       | %FileCheck --check-prefix CHECK-DISABLED %s

// At -Onone, don't expect the lowering to happen.
// RUN: %target-swift-frontend %s -Onone \
// RUN:   -enable-throws-prediction \
// RUN:   -module-name=test -emit-irgen \
// RUN:       | %FileCheck --check-prefix CHECK-DISABLED %s

enum MyError: Error {
  case err
  case ok
  case meh
}

// CHECK-ENABLED-LABEL: define {{.*}}@"${{.*}}condThrows{{.*}}
// CHECK-ENABLED:   br i1 %0, {{.*}} !prof [[WEIGHTS:![0-9]+]]

// CHECK-DISABLED-LABEL: define {{.*}}@"${{.*}}condThrows{{.*}}
// CHECK-DISABLED-NOT:   !prof
public func condThrows(_ b: Bool) throws -> Int {
  if b {
    return 42
  }
  throw MyError.err
}

// CHECK-ENABLED: define {{.*}}@"${{.*}}alwaysThrows{{.*}} #[[ATTRS:[0-9]+]]
// CHECK-DISABLED: define {{.*}}@"${{.*}}alwaysThrows{{.*}} #[[ATTRS:[0-9]+]] {
// CHECK-DISABLED-NOT:   !prof
public func alwaysThrows(_ b: Bool) throws {
  if b {
    throw MyError.err
  } else {
    throw MyError.err
  }
}

public enum Color {
  case cyan
  case magenta
  case yellow
  case key
}

public enum Texture<T> {
  case gradient(T)
  case transparent
  case basic(Color)
}

// CHECK-ENABLED-LABEL: define {{.*}}@"${{.*}}switchThrows1{{.*}}
// CHECK-ENABLED:   !prof

// CHECK-DISABLED-LABEL: define {{.*}}@"${{.*}}switchThrows1{{.*}}
// CHECK-DISABLED-NOT:   !prof
public func switchThrows1(_ e: Color) throws {
  switch e {
  case .cyan: throw MyError.err
  case .magenta: throw MyError.ok
  case .yellow: throw MyError.meh
  case .key: break
  }
}

// NOTE: this one is a multi-payload enum; not yet supported to have !prof on it
// since it uses multiple levels of conditional testing.
public func switchThrows2<T>(_ e: Texture<T>) throws -> T? {
  switch e {
  case let .gradient(t):
    return t
  case .transparent:
    throw MyError.meh
  case .basic(.cyan):
    throw MyError.err
  default:
    return nil
  }
}

public func optionalUnwrap1(_ e: Color?) throws -> Color {
  guard let c = e else {
    throw MyError.err
  }
  return c
}

public func condCast1<T>(_ t: T) throws -> Color {
  guard let c = t as? Color else {
    throw MyError.err
  }
  return c
}

// CHECK-ENABLED-DAG: [[WEIGHTS]] = !{!"branch_weights", i32 2001, i32 2}
// CHECK-ENABLED-DAG: attributes #[[ATTRS]] = {{{.*}}cold{{.*}}}

// CHECK-DISABLED-NOT: attributes #[[ATTRS]] = {{{.*}}cold{{.*}}}
