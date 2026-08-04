// RUN: %target-swift-frontend -primary-file %s -emit-sil -parse-as-library | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend -primary-file %s -emit-sil -O -parse-as-library | %FileCheck %s --check-prefix=SIL-OPT
// RUN: %target-swift-frontend -primary-file %s -emit-ir -parse-as-library | %FileCheck %s --check-prefix=IR

func registerCallback(_ body: () -> Void) {}

// Functions in a generic context can have a '@section', because all functions
// go into a text section.
@section("__TEXT,boot")
func genericBoot<T>(_ value: T) -> T { value }

struct Generic<T> {
  @section("__TEXT,boot")
  func member() {}
}

// A specialization of a generic function goes into the same section as the
// generic function it was specialized from.
public func callGenericBoot() -> Int {
  genericBoot(42)
}

// '@section' is inferred for local functions and closures within a function
// that has a '@section'.
@section("__TEXT,boot")
func firmwareBootEntrypoint() {
  func helper() {}
  helper()

  registerCallback {
    // Inference applies to nested closures, too.
    registerCallback {}
  }

  // '@section(default)' suppresses the inference.
  @section(default)
  func notBoot() {}
  notBoot()

  registerCallback { @section(default) in }

  // An explicit '@section' overrides the inference.
  @section("__TEXT,boot2")
  func elsewhere() {}
  elsewhere()

  registerCallback { @section("__TEXT,boot2") in }
}

// Local types (and their members) do not infer a '@section'.
@section("__TEXT,boot")
func withLocalType() {
  struct Local {
    func member() {}

    var property: Int { 0 }
  }

  Local().member()
  _ = Local().property

  // The accessors of a local variable are code within the enclosing function,
  // so they do infer the section.
  var localProperty: Int { 17 }
  _ = localProperty
}

// There is no inference from a variable to its accessors.
@section("__DATA,boot")
var storedVariable: Int = 0

// Nor from a variable to a closure in its initializer.
var initializedVariable: Int = registerCallbackReturningInt { 0 }

func registerCallbackReturningInt(_ body: () -> Int) -> Int { body() }

// The code of a default argument generator is emitted with the section of the
// function it belongs to, so a closure in a default argument belongs there too.
@section("__TEXT,boot")
func withDefaultArgument(callback: () -> Int = { 0 }) {}

// A synthesized accessor that provides read-only access infers its section from
// an explicitly-written accessor that provides read-only access, and likewise
// for the accessors that write.
struct AccessorInference {
  var viaCoroutines: Int {
    @section("__TEXT,boot") _read { yield 0 }
    @section("__TEXT,boot2") _modify { var x = 0; yield &x }
  }

  var viaGetSet: Int {
    @section("__TEXT,boot") get { 0 }
    @section("__TEXT,boot2") set {}
  }

  // '@section(default)' on the explicitly-written accessor suppresses the
  // inference for the synthesized accessors of the same flavor.
  var suppressed: Int {
    @section("__TEXT,boot") get { 0 }
    @section(default) set {}
  }

  // The property observers can carry a '@section', which is then used for the
  // 'set' that the implementation synthesizes.
  var observedDidSet: Int = 0 {
    @section("__TEXT,boot") didSet {}
  }

  var observedWillSet: Int = 0 {
    @section("__TEXT,boot2") willSet {}
  }
}

// The section applies to initializers and deinitializers as well.
class BootConfig {
  @section("__TEXT,boot") init() {}
  @section("__TEXT,boot") deinit {}
}

// SIL: sil hidden [section "__TEXT,boot"] @$s17section_functions11genericBootyxxlF
// SIL: sil hidden [section "__TEXT,boot"] @$s17section_functions7GenericV6memberyyF

// SIL: sil hidden [section "__TEXT,boot"] @$s17section_functions22firmwareBootEntrypointyyF
// SIL: sil private [section "__TEXT,boot"] @$s17section_functions22firmwareBootEntrypointyyF6helperL_yyF
// SIL: sil private [section "__TEXT,boot"] @$s17section_functions22firmwareBootEntrypointyyFyyXEfU_
// SIL: sil private [section "__TEXT,boot"] @$s17section_functions22firmwareBootEntrypointyyFyyXEfU_yyXEfU_
// SIL: sil private @$s17section_functions22firmwareBootEntrypointyyF03notD0L_yyF
// SIL: sil private @$s17section_functions22firmwareBootEntrypointyyFyyXEfU0_
// SIL: sil private [section "__TEXT,boot2"] @$s17section_functions22firmwareBootEntrypointyyF9elsewhereL_yyF
// SIL: sil private [section "__TEXT,boot2"] @$s17section_functions22firmwareBootEntrypointyyFyyXEfU1_

// SIL: sil hidden [section "__TEXT,boot"] @$s17section_functions13withLocalTypeyyF
// SIL: sil private @$s17section_functions13withLocalTypeyyF0D0L_V6memberyyF
// SIL: sil private @$s17section_functions13withLocalTypeyyF0D0L_V8propertySivg
// SIL: sil private [section "__TEXT,boot"] @$s17section_functions13withLocalTypeyyF13localPropertyL_Sivg

// SIL: sil hidden [global_init] @$s17section_functions14storedVariableSivau
// SIL: sil private @$s17section_functions19initializedVariableSivpfiSiyXEfU_

// Both the default argument generator and the closure written inside it.
// SIL: sil hidden [section "__TEXT,boot"] @$s17section_functions19withDefaultArgument8callbackySiyXE_tFfA_
// SIL: sil private [section "__TEXT,boot"] @$s17section_functions19withDefaultArgument8callbackySiyXE_tFfA_SiycfU_

// The synthesized 'get' comes from '_read', the synthesized 'set' from
// '_modify'.
// SIL: sil hidden [section "__TEXT,boot"] @$s17section_functions17AccessorInferenceV13viaCoroutinesSivr
// SIL: sil hidden [section "__TEXT,boot2"] @$s17section_functions17AccessorInferenceV13viaCoroutinesSivM
// SIL: sil hidden [transparent] [section "__TEXT,boot"] @$s17section_functions17AccessorInferenceV13viaCoroutinesSivg
// SIL: sil hidden [transparent] [section "__TEXT,boot2"] @$s17section_functions17AccessorInferenceV13viaCoroutinesSivs

// The synthesized '_modify' comes from 'set'.
// SIL: sil hidden [section "__TEXT,boot"] @$s17section_functions17AccessorInferenceV9viaGetSetSivg
// SIL: sil hidden [section "__TEXT,boot2"] @$s17section_functions17AccessorInferenceV9viaGetSetSivs
// SIL: sil hidden [transparent] [section "__TEXT,boot2"] @$s17section_functions17AccessorInferenceV9viaGetSetSivM

// SIL: sil hidden [section "__TEXT,boot"] @$s17section_functions17AccessorInferenceV10suppressedSivg
// SIL: sil hidden @$s17section_functions17AccessorInferenceV10suppressedSivs
// SIL: sil hidden [transparent] @$s17section_functions17AccessorInferenceV10suppressedSivM

// The synthesized 'set' and '_modify' come from 'didSet'.
// SIL: sil private [section "__TEXT,boot"] @$s17section_functions17AccessorInferenceV14observedDidSetSivW
// SIL: sil hidden @$s17section_functions17AccessorInferenceV14observedDidSetSivg
// SIL: sil hidden [section "__TEXT,boot"] @$s17section_functions17AccessorInferenceV14observedDidSetSivs
// SIL: sil hidden [section "__TEXT,boot"] @$s17section_functions17AccessorInferenceV14observedDidSetSivM

// The synthesized 'set' and '_modify' come from 'willSet'.
// SIL: sil private [section "__TEXT,boot2"] @$s17section_functions17AccessorInferenceV15observedWillSetSivw
// SIL: sil hidden [transparent] @$s17section_functions17AccessorInferenceV15observedWillSetSivg
// SIL: sil hidden [section "__TEXT,boot2"] @$s17section_functions17AccessorInferenceV15observedWillSetSivs
// SIL: sil hidden [transparent] [section "__TEXT,boot2"] @$s17section_functions17AccessorInferenceV15observedWillSetSivM

// SIL: sil hidden [exact_self_class] [section "__TEXT,boot"] @$s17section_functions10BootConfigCACycfC
// SIL: sil hidden [section "__TEXT,boot"] @$s17section_functions10BootConfigCACycfc
// SIL: sil hidden [section "__TEXT,boot"] @$s17section_functions10BootConfigCfd
// SIL: sil hidden [section "__TEXT,boot"] @$s17section_functions10BootConfigCfD

// The specialization of 'genericBoot' lands in the same section.
// SIL-OPT: sil shared [section "__TEXT,boot"] @$s17section_functions11genericBootyxxlFSi_Tg5

// IR: define {{.*}}@"$s17section_functions11genericBootyxxlF"({{.*}}section "__TEXT,boot"
// IR: define {{.*}}@"$s17section_functions7GenericV6memberyyF"({{.*}}section "__TEXT,boot"
// IR: define {{.*}}@"$s17section_functions22firmwareBootEntrypointyyF"({{.*}}section "__TEXT,boot"
// IR: define {{.*}}@"$s17section_functions22firmwareBootEntrypointyyF6helperL_yyF"({{.*}}section "__TEXT,boot"
// IR: define {{.*}}@"$s17section_functions22firmwareBootEntrypointyyFyyXEfU_"({{.*}}section "__TEXT,boot"
