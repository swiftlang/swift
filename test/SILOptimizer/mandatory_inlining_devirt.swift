// RUN: %target-swift-frontend -sil-verify-all %s -module-name test -Xllvm -sil-print-types -emit-sil -o - -verify | %FileCheck %s --enable-var-scope


// Constructor calls are dispatched dynamically for open classes, even if
// the constructor itself is not "open".

open class OpenClass {
  // CHECK-LABEL: sil @$s4test9OpenClassC1xACSi_tcfC
  // CHECK: [[M:%[0-9]+]] = class_method %1 : $@thick OpenClass.Type, #OpenClass.init!allocator
  // CHECK: apply [[M]]
  // CHECK: return
  public convenience init(x: Int) {
    self.init(x: x, y: 27)
  }
  public init(x: Int, y: Int) {
  }
}

// Static dispatch for not-open class (we are compiling with -wmo).

public class PublicClass {
  // CHECK-LABEL: sil @$s4test11PublicClassC1xACSi_tcfC
  // CHECK: [[M:%[0-9]+]] = function_ref @$s4test11PublicClassC1x1yACSi_SitcfC
  // CHECK: apply [[M]]
  // CHECK: return
  public convenience init(x: Int) {
    self.init(x: x, y: 27)
  }
  public init(x: Int, y: Int) {
  }
}

public protocol Thrower {
  func fail() throws
}

@_transparent public func callee<T : Thrower>(_ t: T) throws {
  try t.fail()
}

public struct Concrete : Thrower {
  public func fail() throws {}
}

// CHECK-LABEL: sil @$s4test6calleryyAA8ConcreteVKF : $@convention(thin) (Concrete) -> @error any Error
public func caller(_ c: Concrete) throws {
  // CHECK: [[FN:%.*]] = function_ref @$s4test8ConcreteV4failyyKF : $@convention(method) (Concrete) -> @error any Error
  // CHECK: try_apply [[FN]](%0) : $@convention(method) (Concrete) -> @error any Error
  try callee(c)
}


public struct File {
  var alias: FileHandle = FileHandle()
}

public class FileHandle {
  @_borrowed var file: File = File()
}

// CHECK-LABEL: sil @$s4test20access_borrowed_readyAA10FileHandleCADF : $@convention(thin) (@guaranteed FileHandle) -> @owned FileHandle {
public func access_borrowed_read(_ l: FileHandle) -> FileHandle {
  // CHECK-NOT: begin_apply
  // CHECK:     return
  return l.file.alias
}
