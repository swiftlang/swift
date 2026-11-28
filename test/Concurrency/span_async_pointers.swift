// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify -strict-concurrency=complete -disable-availability-checking -module-name main -parse-as-library %s
// RUN: %target-swift-frontend -emit-sil -Onone -disable-availability-checking -module-name main -parse-as-library %s | %FileCheck %s --check-prefix=ONONE
// RUN: %target-swift-frontend -emit-sil -O -disable-availability-checking -module-name main -parse-as-library %s | %FileCheck %s --check-prefix=OPT

// REQUIRES: concurrency

@inline(never)
nonisolated(nonsending) func use(_ buffer: UnsafeBufferPointer<Int>) async {}

@inline(never)
nonisolated(nonsending) func use(_ bytes: UnsafeRawBufferPointer) async {}

@inline(never)
nonisolated(nonsending) func use(
  _ buffer: UnsafeMutableBufferPointer<Int>, _ count: inout Int
) async {}

public nonisolated(nonsending) func typedRead(_ span: Span<Int>) async {
  await span.withUnsafeBufferPointer { await use($0) }
}

public nonisolated(nonsending) func rawRead(_ span: RawSpan) async {
  await span.withUnsafeBytes { await use($0) }
}

public nonisolated(nonsending) func output(_ output: inout OutputSpan<Int>) async {
  await unsafe output.withUnsafeMutableBufferPointer { await use($0, &$1) }
}

// Without optimizations, the client has its own copy of each method and of the
// `_withMemoryRebound` helper, so it doesn't need any new entry points.

// ONONE-LABEL: sil @$s4main9typedReadyys4SpanVySiGYaF
// ONONE:         function_ref @$ss4SpanVsRi_zrlE23withUnsafeBufferPointeryqd__qd__SRyxGYaqd_0_YKYCXEYaqd_0_YKs5ErrorRd_0_Ri_d__r0_lF
// ONONE:       } // end sil function

// ONONE-LABEL: sil shared [export_implementation] {{.*}}@$ss4SpanVsRi_zrlE23withUnsafeBufferPointeryqd__qd__SRyxGYaqd_0_YKYCXEYaqd_0_YKs5ErrorRd_0_Ri_d__r0_lF
// ONONE:         function_ref @$ss4SpanVsRi_zrlE14_unsafePointerSVSgvg
// ONONE:         function_ref @$sSWsE18_withMemoryRebound2to_q_xm_q_SRyxGYaq0_YKYCXEtYaq0_YKs5ErrorR0_Ri_zRi__r1_lF
// ONONE:       } // end sil function

// The helper binds the memory, calls `body` once in the isolation of the
// caller, and rebinds the memory once `body` returns or throws.
// ONONE-LABEL: sil shared [export_implementation] {{.*}}@$sSWsE18_withMemoryRebound2to_q_xm_q_SRyxGYaq0_YKYCXEtYaq0_YKs5ErrorR0_Ri_zRi__r1_lF
// ONONE:       bb0({{.*}}, [[ISOLATION:%[0-9]+]] : $Builtin.ImplicitActor, {{.*}}, [[BODY:%[0-9]+]] : $@caller_isolated @noescape @async
// ONONE:         bind_memory
// ONONE:         try_apply [[BODY]]({{%[0-9]+}}, {{%[0-9]+}}, [[ISOLATION]], {{%[0-9]+}})
// ONONE-NOT:     try_apply [[BODY]]
// ONONE:         rebind_memory
// ONONE:       } // end sil function

// ONONE-LABEL: sil shared [export_implementation] {{.*}}@$sSwsE18_withMemoryRebound2to_q_xm_q_SryxGYaq0_YKYCXEtYaq0_YKs5ErrorR0_Ri_zRi__r1_lF

// With optimizations, the methods are inlined. The closure is called once,
// while the memory is bound, and the task never changes its executor.

// OPT-LABEL: sil @$s4main9typedReadyys4SpanVySiGYaF
// OPT-NOT:     hop_to_executor
// OPT-NOT:     function_ref @$ss4SpanV
// OPT:         [[TOKEN:%[0-9]+]] = bind_memory [[BASE:%[0-9]+]], {{%[0-9]+}} to $Int
// OPT-NOT:     hop_to_executor
// OPT:         function_ref @$s4main3useyySRySiGYaF
// OPT-NOT:     function_ref @$s4main3use
// OPT-NOT:     hop_to_executor
// OPT:         rebind_memory
// OPT-NOT:     hop_to_executor
// OPT-NOT:     function_ref @$s4main3use
// OPT:       } // end sil function

// OPT-LABEL: sil @$s4main7rawReadyys7RawSpanVYaF
// OPT-NOT:     hop_to_executor
// OPT-NOT:     function_ref @$ss7RawSpanV
// OPT:         function_ref @$s4main3useyySWYaF
// OPT-NOT:     function_ref @$s4main3use
// OPT-NOT:     hop_to_executor
// OPT:       } // end sil function

// The initialized count is checked against the capacity after the memory is
// rebound, the same as in the synchronous method.
// OPT-LABEL: sil @$s4main6outputyys10OutputSpanVySiGzYaF
// OPT-NOT:     hop_to_executor
// OPT-NOT:     function_ref @$ss10OutputSpanV
// OPT:         bind_memory
// OPT:         function_ref @$s4main3useyySrySiG_SiztYaF
// OPT-NOT:     function_ref @$s4main3use
// OPT:         rebind_memory
// OPT:         cond_fail {{%[0-9]+}}, "OutputSpan capacity overflow"
// OPT-NOT:     hop_to_executor
// OPT:       } // end sil function

func asyncWork() async {}

func syncClosuresUseTheSyncOverloads(_ array: [Int], _ output: inout OutputRawSpan) async {
  // A closure that doesn't await uses the synchronous overload, so the call
  // needs no `await`.
  let count = array.span.withUnsafeBufferPointer { $0.count }
  _ = count
  let byteCount = array.span.bytes.withUnsafeBytes { $0.count }
  _ = byteCount
  unsafe output.withUnsafeMutableBytes { _, _ in }
}

func asyncClosuresUseTheAsyncOverloads(
  _ array: inout [UInt8],
  _ output: inout OutputSpan<UInt8>,
  _ rawOutput: inout OutputRawSpan
) async throws {
  do {
    let span = array.span
    await span.withUnsafeBufferPointer { _ in await asyncWork() }
    await span.withUnsafeBytes { _ in await asyncWork() }
    await span.bytes.withUnsafeBytes { _ in await asyncWork() }

    // Typed throws are propagated.
    struct DecodeError: Error {}
    let _: Int = try await span.withUnsafeBytes { bytes async throws(DecodeError) in
      await asyncWork()
      if bytes.isEmpty { throw DecodeError() }
      return bytes.count
    }
  }

  var mutableSpan = array.mutableSpan
  await mutableSpan.withUnsafeBufferPointer { _ in await asyncWork() }
  await mutableSpan.withUnsafeMutableBufferPointer { _ in await asyncWork() }
  await mutableSpan.withUnsafeBytes { _ in await asyncWork() }
  await mutableSpan.withUnsafeMutableBytes { _ in await asyncWork() }

  var mutableBytes = mutableSpan.mutableBytes
  await mutableBytes.withUnsafeBytes { _ in await asyncWork() }
  await mutableBytes.withUnsafeMutableBytes { _ in await asyncWork() }

  await unsafe output.withUnsafeMutableBufferPointer { _, _ in await asyncWork() }
  await unsafe rawOutput.withUnsafeMutableBytes { _, _ in await asyncWork() }
}

@MainActor
func closuresRunOnTheIsolationOfTheCaller(_ array: [Int]) async {
  // The closure isn't sent anywhere, so it can use non-Sendable state of the
  // caller.
  class NonSendable { var value = 0 }
  let state = NonSendable()
  await array.span.withUnsafeBufferPointer { buffer in
    await asyncWork()
    state.value = buffer.count
  }
  _ = state
}
