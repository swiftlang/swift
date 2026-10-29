// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature Embedded -enable-experimental-feature SendCalls -parse-as-library -wmo -module-name main %s -o %t/out.sil
// RUN: %FileCheck %s --check-prefix=PLAIN < %t/out.sil
// RUN: %FileCheck %s --check-prefix=GLOBAL < %t/out.sil
// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature Embedded -enable-experimental-feature SendCalls -parse-as-library -wmo -module-name main %s -o %t/out.ll
// RUN: %FileCheck %s --check-prefix=IR < %t/out.ll
// RUN: %FileCheck %s --check-prefix=NOFUNCLET < %t/out.ll

// REQUIRES: optimized_stdlib
// REQUIRES: concurrency
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_SendCalls

// In Embedded Swift 'send' on a synchronous method of a plain actor or a
// synchronous function isolated to a global actor is enqueued on the actor
// without any async code at the call site, whether the callee is
// '@oneway' or not

import _Concurrency

actor Counter {
  var n = 0
  func bump(_ by: Int) { n += by }
}

@globalActor actor Background {
  static let shared = Background()
}

@Background var backgroundTotal = 0

@Background func note(_ by: Int) { backgroundTotal += by }

func sendPlain(_ c: Counter, _ x: Int) {
  send c.bump(x)
}

func sendGlobal(_ x: Int) {
  send note(x)
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

// GLOBAL-LABEL: sil {{.*}}@$e4main10sendGlobal{{.*}}F : $@convention(thin) (Int) -> () {
// GLOBAL-NOT: createAsyncTask
// GLOBAL-NOT: hop_to_executor
// GLOBAL: function_ref @{{.*}}25_enqueueOnewayIsolatedAny
// GLOBAL-NOT: createAsyncTask
// GLOBAL-NOT: hop_to_executor
// GLOBAL: end sil function '$e4main10sendGlobal{{.*}}F'

// ==== -----------------------------------------------------------------------
// MARK: IR

// IR-DAG: define {{.*}}@{{"?}}$e4main9sendPlain{{[^"(]*}}F{{"?}}(
// IR-DAG: define {{.*}}@{{"?}}$e4main10sendGlobal{{[^"(]*}}F{{"?}}(
// IR-DAG: call {{.*}}@swift_task_create(

// NOFUNCLET-NOT: sendPlain{{[^" (,]*}}T{{[QY]}}{{[0-9]+}}_
// NOFUNCLET-NOT: sendGlobal{{[^" (,]*}}T{{[QY]}}{{[0-9]+}}_
// NOFUNCLET-NOT: CounterC4bump{{[^" (,]*}}T{{[QY]}}{{[0-9]+}}_
// NOFUNCLET-NOT: 4note{{[^" (,]*}}T{{[QY]}}{{[0-9]+}}_
