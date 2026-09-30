// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature Embedded -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -parse-as-library -wmo -module-name main %s -o %t/out.sil
// RUN: %FileCheck %s --check-prefix=PLAIN < %t/out.sil
// RUN: %FileCheck %s --check-prefix=CLOSURE < %t/out.sil
// RUN: %FileCheck %s --check-prefix=GLOBAL < %t/out.sil
// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature Embedded -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -parse-as-library -wmo -module-name main %s -o %t/out.ll
// RUN: %FileCheck %s --check-prefix=IR < %t/out.ll
// RUN: %FileCheck %s --check-prefix=NOFUNCLET < %t/out.ll

// REQUIRES: optimized_stdlib
// REQUIRES: concurrency
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_OnewayNowait

// In Embedded Swift 'nowait' on a synchronous 'oneway' method of a plain actor
// or of a global actor lowers to a direct call of '_enqueueOneway' /
// '_enqueueOnewayIsolated' with a synchronous closure: no 'createAsyncTask'
// and no 'hop_to_executor' at the call site, and no async funclets in the
// caller or in the enqueued closure

import _Concurrency

actor Counter {
  var n = 0
  func bump(_ by: Int) oneway { n += by }
}

@globalActor actor Background {
  static let shared = Background()
}

@Background var backgroundTotal = 0

@Background func note(_ by: Int) oneway { backgroundTotal += by }

func sendPlain(_ c: Counter, _ x: Int) {
  nowait c.bump(x)
}

func sendGlobal(_ x: Int) {
  nowait note(x)
}

@main struct Main {
  static func main() async {
    let c = Counter()
    sendPlain(c, 1)
    sendGlobal(2)
  }
}

// ==== -----------------------------------------------------------------------
// MARK: SIL

// PLAIN-LABEL: sil {{.*}}@$e4main9sendPlain{{.*}}F : $@convention(thin) (@guaranteed Counter, Int) -> () {
// PLAIN-NOT: createAsyncTask
// PLAIN-NOT: hop_to_executor
// PLAIN: function_ref @{{.*}}14_enqueueOneway2on_
// PLAIN-NOT: createAsyncTask
// PLAIN-NOT: hop_to_executor
// PLAIN: end sil function '$e4main9sendPlain{{.*}}F'

// The enqueued closure is synchronous and isolated to its actor parameter,
// so it calls the method directly
// CLOSURE-LABEL: sil {{.*}}@$e4main9sendPlain{{.*}}U_ : $@convention(thin) {{.*}}(@sil_isolated @guaranteed {{.*}}, Int) -> () {{.*}}{
// CLOSURE-NOT: hop_to_executor
// CLOSURE: function_ref @$e4main7CounterC4bump
// CLOSURE-NOT: hop_to_executor
// CLOSURE: end sil function '$e4main9sendPlain{{.*}}U_'

// GLOBAL-LABEL: sil {{.*}}@$e4main10sendGlobal{{.*}}F : $@convention(thin) (Int) -> () {
// GLOBAL-NOT: createAsyncTask
// GLOBAL-NOT: hop_to_executor
// GLOBAL: function_ref @{{.*}}22_enqueueOnewayIsolated
// GLOBAL-NOT: createAsyncTask
// GLOBAL-NOT: hop_to_executor
// GLOBAL: end sil function '$e4main10sendGlobal{{.*}}F'

// ==== -----------------------------------------------------------------------
// MARK: IR

// IR-DAG: define {{.*}}@{{"?}}$e4main9sendPlain{{[^"(]*}}F{{"?}}(
// IR-DAG: define {{.*}}@{{"?}}$e4main10sendGlobal{{[^"(]*}}F{{"?}}(

// NOFUNCLET-NOT: sendPlain{{[^" (,]*}}T{{[QY]}}{{[0-9]+}}_
// NOFUNCLET-NOT: sendGlobal{{[^" (,]*}}T{{[QY]}}{{[0-9]+}}_
// NOFUNCLET-NOT: CounterC4bump{{[^" (,]*}}T{{[QY]}}{{[0-9]+}}_
// NOFUNCLET-NOT: 4note{{[^" (,]*}}T{{[QY]}}{{[0-9]+}}_
// The operation of the task that '_enqueueOnewayUnchecked' creates never
// suspends or hops, and needs no async reabstraction thunk ('TG5'). The
// resume partial of its async partial application forwarder ('TATQ0_') is
// the one funclet left, since IRGen emits that forwarder as a suspending call
// NOFUNCLET-NOT: onewayOperation{{[^" (,]*}}_T{{[gG]q?}}5T{{[QY]}}{{[0-9]+}}_
// NOFUNCLET-NOT: onewayOperation{{[^" (,]*}}_TG5
