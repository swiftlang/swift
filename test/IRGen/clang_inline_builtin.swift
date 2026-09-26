// RUN: %target-swift-frontend -emit-ir -import-objc-header %S/Inputs/clang_inline_builtin.h %s | %FileCheck %s

// When a C standard library header fortifies a builtin by redefining it with
// __gnu_inline__ and __always_inline__, Clang emits the inline body into a
// private `<name>.inline` clone and leaves the unsuffixed symbol as a
// declaration of the library function, so that taking the address still yields
// the library function. Direct calls have to be redirected to the clone, the
// way Clang does in EmitDirectCallee; otherwise we call the unfortified library
// function and lose the fortification.
//
// sprintf and snprintf are variadic, and Swift cannot call C variadic
// functions, so there is no call site to check for them.

// CHECK-LABEL: define {{.*}}12test_memccpy
func test_memccpy(_ dst: UnsafeMutableRawPointer, _ src: UnsafeRawPointer,
                  _ c: Int32, _ n: Int) {
  // CHECK-NOT: call {{.*}} @memccpy(
  // CHECK: call {{.*}} @__memccpy_chk(
  memccpy(dst, src, c, n)
}

// CHECK-LABEL: define {{.*}}11test_memcpy
func test_memcpy(_ dst: UnsafeMutableRawPointer, _ src: UnsafeRawPointer,
                 _ n: Int) {
  // CHECK-NOT: call {{.*}} @memcpy(
  // CHECK: call {{.*}} @__memcpy_chk(
  memcpy(dst, src, n)
}

// CHECK-LABEL: define {{.*}}12test_memmove
func test_memmove(_ dst: UnsafeMutableRawPointer, _ src: UnsafeRawPointer,
                  _ n: Int) {
  // CHECK-NOT: call {{.*}} @memmove(
  // CHECK: call {{.*}} @__memmove_chk(
  memmove(dst, src, n)
}

// CHECK-LABEL: define {{.*}}11test_memset
func test_memset(_ dst: UnsafeMutableRawPointer, _ c: Int32, _ n: Int) {
  // CHECK-NOT: call {{.*}} @memset(
  // CHECK: call {{.*}} @__memset_chk(
  memset(dst, c, n)
}

// CHECK-LABEL: define {{.*}}11test_stpcpy
func test_stpcpy(_ dst: UnsafeMutablePointer<CChar>,
                 _ src: UnsafePointer<CChar>) {
  // CHECK-NOT: call {{.*}} @stpcpy(
  // CHECK: call {{.*}} @__stpcpy_chk(
  stpcpy(dst, src)
}

// CHECK-LABEL: define {{.*}}12test_stpncpy
func test_stpncpy(_ dst: UnsafeMutablePointer<CChar>,
                  _ src: UnsafePointer<CChar>, _ n: Int) {
  // CHECK-NOT: call {{.*}} @stpncpy(
  // CHECK: call {{.*}} @__stpncpy_chk(
  stpncpy(dst, src, n)
}

// CHECK-LABEL: define {{.*}}11test_strcat
func test_strcat(_ dst: UnsafeMutablePointer<CChar>,
                 _ src: UnsafePointer<CChar>) {
  // CHECK-NOT: call {{.*}} @strcat(
  // CHECK: call {{.*}} @__strcat_chk(
  strcat(dst, src)
}

// CHECK-LABEL: define {{.*}}11test_strcpy
func test_strcpy(_ dst: UnsafeMutablePointer<CChar>,
                 _ src: UnsafePointer<CChar>) {
  // CHECK-NOT: call {{.*}} @strcpy(
  // CHECK: call {{.*}} @__strcpy_chk(
  strcpy(dst, src)
}

// CHECK-LABEL: define {{.*}}12test_strlcat
func test_strlcat(_ dst: UnsafeMutablePointer<CChar>,
                  _ src: UnsafePointer<CChar>, _ n: Int) {
  // CHECK-NOT: call {{.*}} @strlcat(
  // CHECK: call {{.*}} @__strlcat_chk(
  strlcat(dst, src, n)
}

// CHECK-LABEL: define {{.*}}12test_strlcpy
func test_strlcpy(_ dst: UnsafeMutablePointer<CChar>,
                  _ src: UnsafePointer<CChar>, _ n: Int) {
  // CHECK-NOT: call {{.*}} @strlcpy(
  // CHECK: call {{.*}} @__strlcpy_chk(
  strlcpy(dst, src, n)
}

// CHECK-LABEL: define {{.*}}12test_strncat
func test_strncat(_ dst: UnsafeMutablePointer<CChar>,
                  _ src: UnsafePointer<CChar>, _ n: Int) {
  // CHECK-NOT: call {{.*}} @strncat(
  // CHECK: call {{.*}} @__strncat_chk(
  strncat(dst, src, n)
}

// CHECK-LABEL: define {{.*}}12test_strncpy
func test_strncpy(_ dst: UnsafeMutablePointer<CChar>,
                  _ src: UnsafePointer<CChar>, _ n: Int) {
  // CHECK-NOT: call {{.*}} @strncpy(
  // CHECK: call {{.*}} @__strncpy_chk(
  strncpy(dst, src, n)
}

// CHECK-LABEL: define {{.*}}14test_vsnprintf
func test_vsnprintf(_ dst: UnsafeMutablePointer<CChar>, _ n: Int,
                    _ fmt: UnsafePointer<CChar>, _ ap: CVaListPointer) {
  // CHECK-NOT: call {{.*}} @vsnprintf(
  // CHECK: call {{.*}} @__vsnprintf_chk(
  vsnprintf(dst, n, fmt, ap)
}

// CHECK-LABEL: define {{.*}}13test_vsprintf
func test_vsprintf(_ dst: UnsafeMutablePointer<CChar>,
                   _ fmt: UnsafePointer<CChar>, _ ap: CVaListPointer) {
  // CHECK-NOT: call {{.*}} @vsprintf(
  // CHECK: call {{.*}} @__vsprintf_chk(
  vsprintf(dst, fmt, ap)
}
