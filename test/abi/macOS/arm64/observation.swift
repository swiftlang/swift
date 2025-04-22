// RUN: %empty-directory(%t)
// RUN: %llvm-nm -g --defined-only -f just-symbols %stdlib_dir/arm64/libswiftObservation.dylib > %t/symbols
// RUN: %abi-symbol-checker %s %t/symbols
// RUN: diff -u %S/../../Inputs/macOS/arm64/observation/baseline %t/symbols

// REQUIRES: swift_stdlib_no_asserts
// REQUIRES: STDLIB_VARIANT=macosx-arm64
// REQUIRES: observation

// *** DO NOT DISABLE OR XFAIL THIS TEST. *** (See comment below.)

// Welcome, Build Wrangler!
//
// This file lists APIs that have recently changed in a way that potentially
// indicates an ABI- or source-breaking problem.
//
// A failure in this test indicates that there is a potential breaking change in
// the Standard Library. If you observe a failure outside of a PR test, please
// reach out to the Standard Library team directly to make sure this gets
// resolved quickly! If your own PR fails in this test, you probably have an
// ABI- or source-breaking change in your commits. Please go and fix it.
//
// Please DO NOT DISABLE THIS TEST. In addition to ignoring the current set of
// ABI breaks, XFAILing this test also silences any future ABI breaks that may
// land on this branch, which simply generates extra work for the next person
// that picks up the mess.
//
// Instead of disabling this test, you'll need to extend the list of expected
// changes at the bottom. (You'll also need to do this if your own PR triggers
// false positives, or if you have special permission to break things.) You can
// find a diff of what needs to be added in the output of the failed test run.
// The order of lines doesn't matter, and you can also include comments to refer
// to any bugs you filed.
//
// Thank you for your help ensuring the stdlib remains compatible with its past!
//                                            -- Your friendly stdlib engineers

// Observation Symbols

// Observation.ObservationTracking.changed.getter : Swift.AnyKeyPath?
Added: _$s11Observation0A8TrackingV7changeds10AnyKeyPathCSgvg

// property descriptor for Observation.ObservationTracking.changed : Swift.AnyKeyPath?
Added: _$s11Observation0A8TrackingV7changeds10AnyKeyPathCSgvpMV

// static Observation.Observed.untilFinished(@isolated(any) @Sendable () throws(B) -> Observation.Observed<A, B>.Iteration) -> Observation.Observed<A, B>
Added: _$s11Observation8ObservedV13untilFinishedyACyxq_GAC9IterationOyxq__GyYbq_YKYAcFZ

// Observation.Observed.makeAsyncIterator() -> Observation.Observed<A, B>.Iterator
Added: _$s11Observation8ObservedV17makeAsyncIteratorAC0E0Vyxq__GyF

// Observation.Observed.Iterator.next(isolation: isolated Swift.Actor?) async throws(B) -> A?
Added: _$s11Observation8ObservedV8IteratorV4next9isolationxSgScA_pSgYi_tYaq_YKF

// async function pointer to Observation.Observed.Iterator.next(isolation: isolated Swift.Actor?) async throws(B) -> A?
Added: _$s11Observation8ObservedV8IteratorV4next9isolationxSgScA_pSgYi_tYaq_YKFTu

// type metadata accessor for Observation.Observed.Iterator
Added: _$s11Observation8ObservedV8IteratorVMa

// nominal type descriptor for Observation.Observed.Iterator
Added: _$s11Observation8ObservedV8IteratorVMn

// protocol conformance descriptor for Observation.Observed<A, B>.Iterator : Swift.AsyncIteratorProtocol in Observation
Added: _$s11Observation8ObservedV8IteratorVyxq__GScIAAMc

// enum case for Observation.Observed.Iteration.next<A, B where A: Swift.Sendable, B: Swift.Error>(Observation.Observed<A, B>.Iteration.Type) -> (A) -> Observation.Observed<A, B>.Iteration
Added: _$s11Observation8ObservedV9IterationO4nextyAEyxq__GxcAGms8SendableRzs5ErrorR_r0_lFWC

// enum case for Observation.Observed.Iteration.finish<A, B where A: Swift.Sendable, B: Swift.Error>(Observation.Observed<A, B>.Iteration.Type) -> Observation.Observed<A, B>.Iteration
Added: _$s11Observation8ObservedV9IterationO6finishyAEyxq__GAGms8SendableRzs5ErrorR_r0_lFWC

// type metadata accessor for Observation.Observed.Iteration
Added: _$s11Observation8ObservedV9IterationOMa

// nominal type descriptor for Observation.Observed.Iteration
Added: _$s11Observation8ObservedV9IterationOMn

// type metadata accessor for Observation.Observed
Added: _$s11Observation8ObservedVMa

// nominal type descriptor for Observation.Observed
Added: _$s11Observation8ObservedVMn

// Observation.Observed.init(@isolated(any) @Sendable () throws(B) -> A) -> Observation.Observed<A, B>
Added: _$s11Observation8ObservedVyACyxq_GxyYbq_YKYAccfC

// protocol conformance descriptor for Observation.Observed<A, B> : Swift.AsyncSequence in Observation
Added: _$s11Observation8ObservedVyxq_GSciAAMc
