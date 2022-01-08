// RUN: %target-swift-emit-silgen  -parse-as-library %s | %FileCheck %s

// Check that we don't crash when referencing a local function that is 
// declared in unreachable code, for example after a return statement.

// CHECK-LABEL: sil hidden [ossa] @$s34local_declared_in_unreachable_code11globalFunc1SiyF : $@convention(thin) () -> Int {
func globalFunc1() -> Int {
    let i = localFunc()

    // CHECK-LABEL: sil private [ossa] @$s34local_declared_in_unreachable_code11globalFunc1SiyF0A4FuncL_SiyF : $@convention(thin) () -> Int {
    func localFunc() -> Int {
        return 42
    }

    return i
}

// CHECK-LABEL: sil hidden [ossa] @$s34local_declared_in_unreachable_code11globalFunc2SiyF : $@convention(thin) () -> Int {
func globalFunc2() -> Int {
    let i = localFunc()
    return i

    // Declaration after return statement
    // CHECK-LABEL: sil private [ossa] @$s34local_declared_in_unreachable_code11globalFunc2SiyF0A4FuncL_SiyF : $@convention(thin) () -> Int {
    func localFunc() -> Int {
        return 42
    }
}

// CHECK-LABEL: sil [serialized] [ossa] @$s34local_declared_in_unreachable_code11globalFunc3SiyF : $@convention(thin) () -> Int {
@inlinable
func globalFunc3() -> Int {
    let i = localFunc()

    // CHECK-LABEL: sil shared [serializable] [ossa] @$s34local_declared_in_unreachable_code11globalFunc3SiyF0A4FuncL_SiyF : $@convention(thin) () -> Int {
    func localFunc() -> Int {
        return 42
    }

    return i
}

// CHECK-LABEL: sil [serialized] [ossa] @$s34local_declared_in_unreachable_code11globalFunc4SiyF : $@convention(thin) () -> Int {
@inlinable
func globalFunc4() -> Int {
    let i = localFunc()
    return i

    // Declaration after return statement
    // CHECK-LABEL: sil shared [serializable] [ossa] @$s34local_declared_in_unreachable_code11globalFunc4SiyF0A4FuncL_SiyF : $@convention(thin) () -> Int {
    func localFunc() -> Int {
        return 42
    }
}
