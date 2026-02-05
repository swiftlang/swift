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

// static Observation.Observations.untilFinished(@isolated(any) @Sendable () throws(B) -> Observation.Observations<A, B>.Iteration) -> Observation.Observations<A, B>
Added: _$s11Observation12ObservationsV13untilFinishedyACyxq_GAC9IterationOyxq__GyYbq_YKYAcFZ

// Observation.Observations.makeAsyncIterator() -> Observation.Observations<A, B>.Iterator
Added: _$s11Observation12ObservationsV17makeAsyncIteratorAC0E0Vyxq__GyF

// Observation.Observations.Iterator.next(isolation: isolated Swift.Actor?) async throws(B) -> A?
Added: _$s11Observation12ObservationsV8IteratorV4next9isolationxSgScA_pSgYi_tYaq_YKF

// async function pointer to Observation.Observations.Iterator.next(isolation: isolated Swift.Actor?) async throws(B) -> A?
Added: _$s11Observation12ObservationsV8IteratorV4next9isolationxSgScA_pSgYi_tYaq_YKFTu

// type metadata accessor for Observation.Observations.Iterator
Added: _$s11Observation12ObservationsV8IteratorVMa

// nominal type descriptor for Observation.Observations.Iterator
Added: _$s11Observation12ObservationsV8IteratorVMn

// protocol conformance descriptor for Observation.Observations<A, B>.Iterator : Swift.AsyncIteratorProtocol in Observation
Added: _$s11Observation12ObservationsV8IteratorVyxq__GScIAAMc

// enum case for Observation.Observations.Iteration.next<A, B where A: Swift.Sendable, B: Swift.Error>(Observation.Observations<A, B>.Iteration.Type) -> (A) -> Observation.Observations<A, B>.Iteration
Added: _$s11Observation12ObservationsV9IterationO4nextyAEyxq__GxcAGms8SendableRzs5ErrorR_r0_lFWC

// enum case for Observation.Observations.Iteration.finish<A, B where A: Swift.Sendable, B: Swift.Error>(Observation.Observations<A, B>.Iteration.Type) -> Observation.Observations<A, B>.Iteration
Added: _$s11Observation12ObservationsV9IterationO6finishyAEyxq__GAGms8SendableRzs5ErrorR_r0_lFWC

// type metadata accessor for Observation.Observations.Iteration
Added: _$s11Observation12ObservationsV9IterationOMa

// nominal type descriptor for Observation.Observations.Iteration
Added: _$s11Observation12ObservationsV9IterationOMn

// type metadata accessor for Observation.Observations
Added: _$s11Observation12ObservationsVMa

// nominal type descriptor for Observation.Observations
Added: _$s11Observation12ObservationsVMn

// Observation.Observations.init(@isolated(any) @Sendable () throws(B) -> A) -> Observation.Observations<A, B>
Added: _$s11Observation12ObservationsVyACyxq_GxyYbq_YKYAccfC

// protocol conformance descriptor for Observation.Observations<A, B> : Swift.AsyncSequence in Observation
Added: _$s11Observation12ObservationsVyxq_GSciAAMc

// static Observation.ObservationTracking.Event.Kind.== infix(Observation.ObservationTracking.Event.Kind, Observation.ObservationTracking.Event.Kind) -> Swift.Bool
Added: _$s11Observation0A8TrackingV5EventV4KindV2eeoiySbAG_AGtFZ

// static Observation.ObservationTracking.Event.Kind.deinit.getter : Observation.ObservationTracking.Event.Kind
Added: _$s11Observation0A8TrackingV5EventV4KindV6deinitAGvgZ

// property descriptor for static Observation.ObservationTracking.Event.Kind.deinit : Observation.ObservationTracking.Event.Kind
Added: _$s11Observation0A8TrackingV5EventV4KindV6deinitAGvpZMV

// static Observation.ObservationTracking.Event.Kind.didSet.getter : Observation.ObservationTracking.Event.Kind
Added: _$s11Observation0A8TrackingV5EventV4KindV6didSetAGvgZ

// property descriptor for static Observation.ObservationTracking.Event.Kind.didSet : Observation.ObservationTracking.Event.Kind
Added: _$s11Observation0A8TrackingV5EventV4KindV6didSetAGvpZMV

// static Observation.ObservationTracking.Event.Kind.initial.getter : Observation.ObservationTracking.Event.Kind
Added: _$s11Observation0A8TrackingV5EventV4KindV7initialAGvgZ

// property descriptor for static Observation.ObservationTracking.Event.Kind.initial : Observation.ObservationTracking.Event.Kind
Added: _$s11Observation0A8TrackingV5EventV4KindV7initialAGvpZMV

// static Observation.ObservationTracking.Event.Kind.willSet.getter : Observation.ObservationTracking.Event.Kind
Added: _$s11Observation0A8TrackingV5EventV4KindV7willSetAGvgZ

// property descriptor for static Observation.ObservationTracking.Event.Kind.willSet : Observation.ObservationTracking.Event.Kind
Added: _$s11Observation0A8TrackingV5EventV4KindV7willSetAGvpZMV

// type metadata accessor for Observation.ObservationTracking.Event.Kind
Added: _$s11Observation0A8TrackingV5EventV4KindVMa

// nominal type descriptor for Observation.ObservationTracking.Event.Kind
Added: _$s11Observation0A8TrackingV5EventV4KindVMn

// type metadata for Observation.ObservationTracking.Event.Kind
Added: _$s11Observation0A8TrackingV5EventV4KindVN

// protocol conformance descriptor for Observation.ObservationTracking.Event.Kind : Swift.Equatable in Observation
Added: _$s11Observation0A8TrackingV5EventV4KindVSQAAMc

// Observation.ObservationTracking.Event.kind.getter : Observation.ObservationTracking.Event.Kind
Added: _$s11Observation0A8TrackingV5EventV4kindAE4KindVvg

// Observation.ObservationTracking.Event.cancel() -> ()
Added: _$s11Observation0A8TrackingV5EventV6cancelyyF

// Observation.ObservationTracking.Event.matches<A where A: Observation.Observable>(Swift.PartialKeyPath<A>) -> Swift.Bool
Added: _$s11Observation0A8TrackingV5EventV7matchesySbs14PartialKeyPathCyxGAA10ObservableRzlF

// type metadata accessor for Observation.ObservationTracking.Event
Added: _$s11Observation0A8TrackingV5EventVMa

// nominal type descriptor for Observation.ObservationTracking.Event
Added: _$s11Observation0A8TrackingV5EventVMn

// type metadata for Observation.ObservationTracking.Event
Added: _$s11Observation0A8TrackingV5EventVN

// Observation.ObservationTracking.Token.cancel() -> ()
Added: _$s11Observation0A8TrackingV5TokenV6cancelyyF

// type metadata accessor for Observation.ObservationTracking.Token
Added: _$s11Observation0A8TrackingV5TokenVMa

// nominal type descriptor for Observation.ObservationTracking.Token
Added: _$s11Observation0A8TrackingV5TokenVMn

// type metadata for Observation.ObservationTracking.Token
Added: _$s11Observation0A8TrackingV5TokenVN

// Observation.ObservationTracking.Token.deinit
Added: _$s11Observation0A8TrackingV5TokenVfD

// Observation.ObservationTracking.Options.init(arrayLiteral: Observation.ObservationTracking.Options...) -> Observation.ObservationTracking.Options
Added: _$s11Observation0A8TrackingV7OptionsV12arrayLiteralA2Ed_tcfC

// Observation.ObservationTracking.Options.intersection(Observation.ObservationTracking.Options) -> Observation.ObservationTracking.Options
Added: _$s11Observation0A8TrackingV7OptionsV12intersectionyA2EF

// Observation.ObservationTracking.Options.formIntersection(Observation.ObservationTracking.Options) -> ()
Added: _$s11Observation0A8TrackingV7OptionsV16formIntersectionyyAEF

// Observation.ObservationTracking.Options.symmetricDifference(Observation.ObservationTracking.Options) -> Observation.ObservationTracking.Options
Added: _$s11Observation0A8TrackingV7OptionsV19symmetricDifferenceyA2EF

// Observation.ObservationTracking.Options.formSymmetricDifference(Observation.ObservationTracking.Options) -> ()
Added: _$s11Observation0A8TrackingV7OptionsV23formSymmetricDifferenceyyAEF

// static Observation.ObservationTracking.Options.== infix(Observation.ObservationTracking.Options, Observation.ObservationTracking.Options) -> Swift.Bool
Added: _$s11Observation0A8TrackingV7OptionsV2eeoiySbAE_AEtFZ

// Observation.ObservationTracking.Options.union(Observation.ObservationTracking.Options) -> Observation.ObservationTracking.Options
Added: _$s11Observation0A8TrackingV7OptionsV5unionyA2EF

// static Observation.ObservationTracking.Options.deinit.getter : Observation.ObservationTracking.Options
Added: _$s11Observation0A8TrackingV7OptionsV6deinitAEvgZ

// property descriptor for static Observation.ObservationTracking.Options.deinit : Observation.ObservationTracking.Options
Added: _$s11Observation0A8TrackingV7OptionsV6deinitAEvpZMV

// static Observation.ObservationTracking.Options.didSet.getter : Observation.ObservationTracking.Options
Added: _$s11Observation0A8TrackingV7OptionsV6didSetAEvgZ

// property descriptor for static Observation.ObservationTracking.Options.didSet : Observation.ObservationTracking.Options
Added: _$s11Observation0A8TrackingV7OptionsV6didSetAEvpZMV

// Observation.ObservationTracking.Options.insert(Observation.ObservationTracking.Options) -> (inserted: Swift.Bool, memberAfterInsert: Observation.ObservationTracking.Options)
Added: _$s11Observation0A8TrackingV7OptionsV6insertySb8inserted_AE17memberAfterInserttAEF

// Observation.ObservationTracking.Options.remove(Observation.ObservationTracking.Options) -> Observation.ObservationTracking.Options?
Added: _$s11Observation0A8TrackingV7OptionsV6removeyAESgAEF

// Observation.ObservationTracking.Options.update(with: Observation.ObservationTracking.Options) -> Observation.ObservationTracking.Options?
Added: _$s11Observation0A8TrackingV7OptionsV6update4withAESgAE_tF

// static Observation.ObservationTracking.Options.willSet.getter : Observation.ObservationTracking.Options
Added: _$s11Observation0A8TrackingV7OptionsV7willSetAEvgZ

// property descriptor for static Observation.ObservationTracking.Options.willSet : Observation.ObservationTracking.Options
Added: _$s11Observation0A8TrackingV7OptionsV7willSetAEvpZMV

// Observation.ObservationTracking.Options.contains(Observation.ObservationTracking.Options) -> Swift.Bool
Added: _$s11Observation0A8TrackingV7OptionsV8containsySbAEF

// Observation.ObservationTracking.Options.formUnion(Observation.ObservationTracking.Options) -> ()
Added: _$s11Observation0A8TrackingV7OptionsV9formUnionyyAEF

// Observation.ObservationTracking.Options.init() -> Observation.ObservationTracking.Options
Added: _$s11Observation0A8TrackingV7OptionsVAEycfC

// type metadata accessor for Observation.ObservationTracking.Options
Added: _$s11Observation0A8TrackingV7OptionsVMa

// nominal type descriptor for Observation.ObservationTracking.Options
Added: _$s11Observation0A8TrackingV7OptionsVMn

// type metadata for Observation.ObservationTracking.Options
Added: _$s11Observation0A8TrackingV7OptionsVN

// protocol conformance descriptor for Observation.ObservationTracking.Options : Swift.Equatable in Observation
Added: _$s11Observation0A8TrackingV7OptionsVSQAAMc

// protocol conformance descriptor for Observation.ObservationTracking.Options : Swift.SetAlgebra in Observation
Added: _$s11Observation0A8TrackingV7OptionsVs10SetAlgebraAAMc

// protocol conformance descriptor for Observation.ObservationTracking.Options : Swift.ExpressibleByArrayLiteral in Observation
Added: _$s11Observation0A8TrackingV7OptionsVs25ExpressibleByArrayLiteralAAMc

// Observation.withContinuousObservation(options: Observation.ObservationTracking.Options, apply: @isolated(any) @Sendable (Observation.ObservationTracking.Event) -> ()) -> Observation.ObservationTracking.Token
Added: _$s11Observation014withContinuousA07options5applyAA0A8TrackingV5TokenVAF7OptionsV_yAF5EventVYbYActF

// Observation.withObservationTracking<A, B where B: Swift.Error, A: ~Swift.Copyable>(options: Observation.ObservationTracking.Options, _: () throws(B) -> A, onChange: @Sendable (Observation.ObservationTracking.Event) -> ()) throws(B) -> A
Added: _$s11Observation04withA8Tracking7options_8onChangexAA0aC0V7OptionsV_xyq_YKXEyAF5EventVYbctq_YKs5ErrorR_Ri_zr0_lF