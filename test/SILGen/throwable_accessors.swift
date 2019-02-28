// RUN: %target-swift-emit-silgen -module-name throwable_accessors -Xllvm -sil-full-demangle %s | %FileCheck %s

enum ThrowableAccessorError: Error {
  case notImplemented
  case lessThanZero
}

struct S {
  private var _storage: Int = 0
  var foo: Int {
    // CHECK: sil hidden [ossa] @$s19throwable_accessors1SV3fooSivg : $@convention(method) (S) -> (Int, @error Error) {
    // CHECK: bb0(%0 : $S):
    // CHECK-NEXT: debug_value %0 : $S, let, name "self", argno 1
    // CHECK-NEXT: debug_value undef : $Error, var, name "$error", argno 2
    // CHECK-NEXT: %3 = alloc_stack $Error
    // CHECK-NEXT: %4 = metatype $@thin ThrowableAccessorError.Type
    // CHECK-NEXT: %5 = enum $ThrowableAccessorError, #ThrowableAccessorError.notImplemented!enumelt
    // CHECK-NEXT: %6 = alloc_existential_box $Error, $ThrowableAccessorError
    // CHECK-NEXT: %7 = project_existential_box $ThrowableAccessorError in %6 : $Error
    // CHECK-NEXT: store %6 to [init] %3 : $*Error
    // CHECK-NEXT: store %5 to [trivial] %7 : $*ThrowableAccessorError
    // CHECK-NEXT: %10 = load [take] %3 : $*Error
    // CHECK-NEXT: %11 = builtin "willThrow"(%10 : $Error) : $()
    // CHECK-NEXT: dealloc_stack %3 : $*Error
    // CHECK-NEXT: throw %10 : $Error
    // CHECK-END: }
    get throws {
      throw ThrowableAccessorError.notImplemented
    }

    // CHECK: sil hidden [ossa] @$s19throwable_accessors1SV3fooSivs : $@convention(method) (Int, @inout S) -> @error Error {
    // CHECK: bb1:
    // CHECK-NEXT: %14 = alloc_stack $Error
    // CHECK-NEXT: %15 = metatype $@thin ThrowableAccessorError.Type
    // CHECK-NEXT: %16 = enum $ThrowableAccessorError, #ThrowableAccessorError.lessThanZero!enumelt
    // CHECK-NEXT: %17 = alloc_existential_box $Error, $ThrowableAccessorError
    // CHECK-NEXT: %18 = project_existential_box $ThrowableAccessorError in %17 : $Error
    // CHECK-NEXT: store %17 to [init] %14 : $*Error
    // CHECK-NEXT: store %16 to [trivial] %18 : $*ThrowableAccessorError
    // CHECK-NEXT: %21 = load [take] %14 : $*Error
    // CHECK-NEXT: %22 = builtin "willThrow"(%21 : $Error) : $()
    // CHECK-NEXT: dealloc_stack %14 : $*Error
    // CHECK-NEXT: throw %21 : $Error
    set throws {
      if newValue < 0 {
        throw ThrowableAccessorError.lessThanZero 
      } else {
        _storage = newValue
      }
    }
  }
}

struct S1 {
  var bar: Int {
    // CHECK: sil hidden [ossa] @$s19throwable_accessors2S1V3barSivg : $@convention(method) (S1) -> (Int, @error Error) {
    // CHECK-NEXT: bb0(%0 : $S1):
    // CHECK-NEXT: debug_value %0 : $S1, let, name "self", argno 1
    // CHECK-NEXT: debug_value undef : $Error, var, name "$error", argno 2
    // CHECK-NEXT: %3 = integer_literal $Builtin.IntLiteral, 0
    // CHECK-NEXT: %4 = metatype $@thin Int.Type
    // CHECK-NEXT: %5 = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
    // CHECK-NEXT: %6 = apply %5(%3, %4) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
    // CHECK-NEXT: return %6 : $Int
    // CHECK-END: }
    get throws {
      return 0
    }
  }
}

var instance: S = S()
let anotherInstance: S1 = S1()

// CHECK: %18 = begin_access [modify] [dynamic] %3 : $*S
// CHECK-NEXT: %19 = function_ref @$s19throwable_accessors1SV3fooSivs : $@convention(method) (Int, @inout S) -> @error Error
// CHECK-NEXT: try_apply %19(%17, %18) : $@convention(method) (Int, @inout S) -> @error Error, normal bb1, error bb5

// CHECK: bb1(%21 : $()):
// CHECK-NEXT: end_access %18 : $*S
// CHECK-NEXT: alloc_global @$s19throwable_accessors5valueSivp
// CHECK-NEXT: %24 = global_addr @$s19throwable_accessors5valueSivp : $*Int
// CHECK-NEXT: %25 = load [trivial] %9 : $*S1
// CHECK-NEXT: %26 = function_ref @$s19throwable_accessors2S1V3barSivg : $@convention(method) (S1) -> (Int, @error Error)
// CHECK-NEXT: try_apply %26(%25) : $@convention(method) (S1) -> (Int, @error Error), normal bb2, error bb6

// CHECK: bb2(%28 : $Int):
// CHECK-NEXT: store %28 to [trivial] %24 : $*Int
// CHECK-NEXT: %30 = integer_literal $Builtin.Int32, 0
// CHECK-NEXT: %31 = struct $Int32 (%30 : $Builtin.Int32)
// CHECK-NEXT: br bb3(%31 : $Int32)

// CHECK: bb3(%33 : $Int32):
// CHECK-NEXT: return %33 : $Int32

// CHECK: bb4(%35 : @owned $Error):
// CHECK-NEXT: %36 = builtin "errorInMain"(%35 : $Error) : $()
// CHECK-NEXT: end_lifetime %35 : $Error
// CHECK-NEXT: %38 = integer_literal $Builtin.Int32, 1
// CHECK-NEXT: %39 = struct $Int32 (%38 : $Builtin.Int32)
// CHECK-NEXT: br bb3(%39 : $Int32)

// CHECK: bb5(%41 : @owned $Error):
// CHECK-NEXT: end_access %18 : $*S
// CHECK-NEXT: br bb4(%41 : $Error)

// CHECK: bb6(%44 : @owned $Error):
// CHECK-NEXT: br bb4(%44 : $Error)
try instance.foo = 1
let value = try anotherInstance.bar