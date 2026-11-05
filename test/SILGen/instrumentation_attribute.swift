// RUN: %target-swift-emit-silgen -parse-as-library -module-name main \
// RUN:   %s | %FileCheck %s


func take(_ body: () -> Void) { body() }
func takeInt(_ body: () -> Int) -> Int { body() }

// CHECK-LABEL: sil hidden [ossa] @$s4main7plainFnyyF :
// CHECK-NOT: no_instrumentation
// CHECK: } // end sil function '$s4main7plainFnyyF'
func plainFn() {}

// CHECK-LABEL: sil hidden [no_instrumentation_address] [ossa] @$s4main9noAddressyyF :
@instrumentation(disable: address)
func noAddress() {}

// CHECK-LABEL: sil hidden [no_instrumentation_thread] [ossa] @$s4main8noThreadyyF :
@instrumentation(disable: thread)
func noThread() {}

// CHECK-LABEL: sil hidden [no_instrumentation_memtag_stack] [ossa] @$s4main8noMemTagyyF :
@instrumentation(disable: memtagStack)
func noMemTag() {}

// CHECK-LABEL: sil hidden [no_instrumentation_coverage] [ossa] @$s4main10noCoverageyyF :
@instrumentation(disable: coverage)
func noCoverage() {}

// A list and stacked attributes are equivalent.
// CHECK-LABEL: sil hidden [no_instrumentation_address] [no_instrumentation_thread] [ossa] @$s4main4listyyF :
@instrumentation(disable: address, thread)
func list() {}

// CHECK-LABEL: sil hidden [no_instrumentation_address] [no_instrumentation_thread] [ossa] @$s4main7stackedyyF :
@instrumentation(disable: address)
@instrumentation(disable: thread)
func stacked() {}

// 'default' on its own disables nothing.
// CHECK-LABEL: sil hidden [ossa] @$s4main9defaultFnyyF :
@instrumentation(default)
func defaultFn() {}

// CHECK-LABEL: sil hidden [no_instrumentation_thread] [ossa] @$s4main17defaultAndDisableyyF :
@instrumentation(default, disable: thread)
func defaultAndDisable() {}

// Closures inherit from the enclosing function.
@instrumentation(disable: address)
func outer() {
  // CHECK-LABEL: sil private [no_instrumentation_address] [ossa] @$s4main5outeryyFyyXEfU_ :
  take { }

  // CHECK-LABEL: sil private [no_instrumentation_address] [ossa] @$s4main5outeryyFyyXEfU0_ :
  take { take { } }

  // 'default' undoes the propagation.
  // CHECK-LABEL: sil private [ossa] @$s4main5outeryyFyyXEfU1_ :
  take { @instrumentation(default) in }

  // ... and can be combined with 'disable:'.
  // CHECK-LABEL: sil private [no_instrumentation_thread] [ossa] @$s4main5outeryyFyyXEfU2_ :
  take { @instrumentation(default, disable: thread) in }

  // An explicit 'disable:' adds to what was inherited.
  // CHECK-LABEL: sil private [no_instrumentation_address] [no_instrumentation_coverage] [ossa] @$s4main5outeryyFyyXEfU3_ :
  take { @instrumentation(disable: coverage) in }

  // A local function inherits as well.
  // CHECK-LABEL: sil private [no_instrumentation_address] [ossa] @$s4main5outeryyF5localL_yyF :
  func local() {}
  local()

  // Nested closures inherit from their closest enclosing closure.
  // CHECK-LABEL: sil private [ossa] @$s4main5outeryyFyyXEfU4_ :
  take { @instrumentation(default) in
    // CHECK-LABEL: sil private [ossa] @$s4main5outeryyFyyXEfU4_yyXEfU_ :
    take { }
  }
}

// A closure in a function that is not itself uninstrumented.
func plainOuter() {
  // CHECK-LABEL: sil private [no_instrumentation_address] [ossa] @$s4main10plainOuteryyFyyXEfU_ :
  take { @instrumentation(disable: address) in }
  // CHECK-LABEL: sil private [ossa] @$s4main10plainOuteryyFyyXEfU0_ :
  take { }
}

struct S: ~Copyable {
  // CHECK-LABEL: sil hidden [no_instrumentation_address] [ossa] @$s4main1SVACycfC :
  @instrumentation(disable: address) init() {}

  // CHECK-LABEL: sil hidden [no_instrumentation_address] [ossa] @$s4main1SVfD :
  @instrumentation(disable: address) deinit {}

  // CHECK-LABEL: sil hidden [no_instrumentation_address] [ossa] @$s4main1SV6methodyyF :
  @instrumentation(disable: address) func method() {}

  // CHECK-LABEL: sil hidden [no_instrumentation_address] [ossa] @$s4main1SV6statusSivg :
  // CHECK-LABEL: sil hidden [no_instrumentation_thread] [ossa] @$s4main1SV6statusSivs :
  var status: Int {
    @instrumentation(disable: address) get { 0 }
    @instrumentation(disable: thread) set {}
  }

  // The attribute on a subscript applies to all of its accessors.
  // CHECK-LABEL: sil hidden [no_instrumentation_address] [ossa] @$s4main1SVyS2icig :
  // CHECK-LABEL: sil hidden [no_instrumentation_address] [ossa] @$s4main1SVyS2icis :
  @instrumentation(disable: address)
  subscript(i: Int) -> Int {
    get { i }
    set {}
  }
}

struct DefaultArg {
  // A closure in the default argument of a subscript inherits from the
  // subscript.
  // CHECK-LABEL: sil private [no_instrumentation_address] [ossa] @$s4main10DefaultArgVyS2i_SiyXEtcipfA0_SiycfU_ :
  @instrumentation(disable: address)
  subscript(i: Int, f: () -> Int = { 0 }) -> Int { i }
}

// Everything lexically inside an uninstrumented function is uninstrumented,
// including the members of types declared in it.
@instrumentation(disable: address)
func outerWithLocalTypes() {
  struct Local {
    // CHECK-DAG: sil private [no_instrumentation_address] [ossa] @$s4main19outerWithLocalTypesyyF0D0L_V6methodyyF :
    func method() {
      // CHECK-DAG: sil private [no_instrumentation_address] [ossa] @$s4main19outerWithLocalTypesyyF0D0L_V6methodyyFyyXEfU_ :
      take { }
    }
  }
  enum E {
    // CHECK-DAG: sil private [no_instrumentation_address] [ossa] @$s4main19outerWithLocalTypesyyF1EL_O1ayADSicADmFfA_SiycfU_ :
    case a(Int = { 0 }())
  }
  Local().method()
  _ = E.a()
}

// A type's members are not affected by an unrelated type's function.
struct Unrelated {
  // CHECK-DAG: sil hidden [ossa] @$s4main9UnrelatedV6methodyyF :
  func method() {}
}

struct ResetInAccessor {
  // An accessor can reset what it inherits from its subscript.
  // CHECK-DAG: sil hidden [ossa] @$s4main15ResetInAccessorVyS2icig :
  // CHECK-DAG: sil hidden [no_instrumentation_address] [ossa] @$s4main15ResetInAccessorVyS2icis :
  @instrumentation(disable: address)
  subscript(i: Int) -> Int {
    @instrumentation(default) get { i }
    set {}
  }
}
