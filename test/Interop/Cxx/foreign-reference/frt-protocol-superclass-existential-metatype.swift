// An existential *metatype* whose class bound is a foreign reference type, as
// in `any P.Type` where `protocol P: SomeFRT`.

// RUN: %target-swift-frontend -emit-silgen -I %S/Inputs -cxx-interoperability-mode=default -disable-availability-checking %s | %FileCheck %s --check-prefix=SIL --implicit-check-not=upcast
// RUN: %target-swift-emit-ir -I %S/Inputs -cxx-interoperability-mode=default -disable-availability-checking -validate-tbd-against-ir=none %s | %FileCheck %s --check-prefix=IR

import FRTProtocolSuperclass

protocol MetaTagged: SharedBase {
  static func describe() -> CInt
}

extension SharedBase: MetaTagged {
  static func describe() -> CInt { 7 }
}

// The type in a signature only, so nothing but lowering runs.
func signatureOnly(_: any MetaTagged.Type) {}

// Produce a value of the existential metatype.
func erase() -> any MetaTagged.Type { SharedBase.self }

// SIL-LABEL: sil hidden [ossa] @{{.*}}5erase{{.*}}
// SIL:         metatype $@thick SharedBase.Type
// SIL:         init_existential_metatype

// Dispatch a member of the class bound through the existential metatype.
func dispatch() -> CInt {
  let m: any MetaTagged.Type = SharedBase.self
  return m.describe()
}

// The opened archetype's metatype is @thick and `describe()` wants @thin, so
// the conversion must be a fresh `metatype` and not an `upcast`.

// SIL-LABEL: sil hidden [ossa] @{{.*}}8dispatchs5Int32VyF
// SIL:         metatype $@thin SharedBase.Type
// SIL:         open_existential_metatype
// SIL:         function_ref @{{.*}}8describe
// SIL:         return

// A @thin metatype occupies nothing, so `describe()` takes no argument at all.

// IR-LABEL: define {{.*}}8dispatch
// IR:         call {{.*}}8describe{{[^(]*}}()

// Everything above is internal, so keep it reachable.
public func driver() -> CInt {
  signatureOnly(erase())
  return dispatch()
}
