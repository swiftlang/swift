// RUN: %empty-directory(%t)
// RUN: %llvm-nm -g --defined-only -f just-symbols %stdlib_dir/arm64/libswiftSynchronization.dylib > %t/symbols
// RUN: %abi-symbol-checker %s %t/symbols
// RUN: diff -u %S/../../Inputs/macOS/arm64/synchronization/baseline %t/symbols

// REQUIRES: swift_stdlib_no_asserts
// REQUIRES: STDLIB_VARIANT=macosx-arm64

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

//===----------------------------------------------------------------------===//
// Synchronization Symbols
//===----------------------------------------------------------------------===//

// protocol descriptor for Synchronization.AtomicValue
Added: _$s15Synchronization11AtomicValueMp

// dispatch thunk of static Synchronization.AtomicValue.decodeAtomicRepresentation(__owned A.AtomicRepresentation) -> A
Added: _$s15Synchronization11AtomicValueP06decodeB14Representationyx0bE0QznFZTj

// method descriptor for static Synchronization.AtomicValue.decodeAtomicRepresentation(__owned A.AtomicRepresentation) -> A
Added: _$s15Synchronization11AtomicValueP06decodeB14Representationyx0bE0QznFZTq

// dispatch thunk of static Synchronization.AtomicValue.encodeAtomicRepresentation(__owned A) -> A.AtomicRepresentation
Added: _$s15Synchronization11AtomicValueP06encodeB14Representationy0bE0QzxnFZTj

// method descriptor for static Synchronization.AtomicValue.encodeAtomicRepresentation(__owned A) -> A.AtomicRepresentation
Added: _$s15Synchronization11AtomicValueP06encodeB14Representationy0bE0QzxnFZTq

// protocol requirements base descriptor for Synchronization.AtomicValue
Added: _$s15Synchronization11AtomicValueTL

// Synchronization.AtomicLoadOrdering.description.getter : Swift.String
Added: _$s15Synchronization18AtomicLoadOrderingV11descriptionSSvg

// property descriptor for Synchronization.AtomicLoadOrdering.description : Swift.String
Added: _$s15Synchronization18AtomicLoadOrderingV11descriptionSSvpMV

// static Synchronization.AtomicLoadOrdering.== infix(Synchronization.AtomicLoadOrdering, Synchronization.AtomicLoadOrdering) -> Swift.Bool
Added: _$s15Synchronization18AtomicLoadOrderingV2eeoiySbAC_ACtFZ

// Synchronization.AtomicLoadOrdering.hash(into: inout Swift.Hasher) -> ()
Added: _$s15Synchronization18AtomicLoadOrderingV4hash4intoys6HasherVz_tF

// Synchronization.AtomicLoadOrdering.init(_rawValue: Swift.Int) -> Synchronization.AtomicLoadOrdering
Added: _$s15Synchronization18AtomicLoadOrderingV9_rawValueACSi_tcfC

// Synchronization.AtomicLoadOrdering._rawValue.modify : Swift.Int
Added: _$s15Synchronization18AtomicLoadOrderingV9_rawValueSivM

// Synchronization.AtomicLoadOrdering._rawValue.getter : Swift.Int
Added: _$s15Synchronization18AtomicLoadOrderingV9_rawValueSivg

// property descriptor for Synchronization.AtomicLoadOrdering._rawValue : Swift.Int
Added: _$s15Synchronization18AtomicLoadOrderingV9_rawValueSivpMV

// Synchronization.AtomicLoadOrdering._rawValue.setter : Swift.Int
Added: _$s15Synchronization18AtomicLoadOrderingV9_rawValueSivs

// Synchronization.AtomicLoadOrdering.hashValue.getter : Swift.Int
Added: _$s15Synchronization18AtomicLoadOrderingV9hashValueSivg

// property descriptor for Synchronization.AtomicLoadOrdering.hashValue : Swift.Int
Added: _$s15Synchronization18AtomicLoadOrderingV9hashValueSivpMV

// type metadata accessor for Synchronization.AtomicLoadOrdering
Added: _$s15Synchronization18AtomicLoadOrderingVMa

// nominal type descriptor for Synchronization.AtomicLoadOrdering
Added: _$s15Synchronization18AtomicLoadOrderingVMn

// type metadata for Synchronization.AtomicLoadOrdering
Added: _$s15Synchronization18AtomicLoadOrderingVN

// protocol conformance descriptor for Synchronization.AtomicLoadOrdering : Swift.Hashable in Synchronization
Added: _$s15Synchronization18AtomicLoadOrderingVSHAAMc

// protocol conformance descriptor for Synchronization.AtomicLoadOrdering : Swift.Equatable in Synchronization
Added: _$s15Synchronization18AtomicLoadOrderingVSQAAMc

// protocol conformance descriptor for Synchronization.AtomicLoadOrdering : Swift.CustomStringConvertible in Synchronization
Added: _$s15Synchronization18AtomicLoadOrderingVs23CustomStringConvertibleAAMc

// Synchronization._Atomic8BitStorage._storage.modify : Builtin.Int8
Added: _$s15Synchronization18_Atomic8BitStorageV8_storageBi8_vM

// Synchronization._Atomic8BitStorage._storage.getter : Builtin.Int8
Added: _$s15Synchronization18_Atomic8BitStorageV8_storageBi8_vg

// property descriptor for Synchronization._Atomic8BitStorage._storage : Builtin.Int8
Added: _$s15Synchronization18_Atomic8BitStorageV8_storageBi8_vpMV

// Synchronization._Atomic8BitStorage._storage.setter : Builtin.Int8
Added: _$s15Synchronization18_Atomic8BitStorageV8_storageBi8_vs

// type metadata accessor for Synchronization._Atomic8BitStorage
Added: _$s15Synchronization18_Atomic8BitStorageVMa

// nominal type descriptor for Synchronization._Atomic8BitStorage
Added: _$s15Synchronization18_Atomic8BitStorageVMn

// type metadata for Synchronization._Atomic8BitStorage
Added: _$s15Synchronization18_Atomic8BitStorageVN

// Synchronization.AtomicLazyReference.storeIfNil(__owned A) -> A
Added: _$s15Synchronization19AtomicLazyReferenceV10storeIfNilyxxnF

// Synchronization.AtomicLazyReference.load() -> A?
Added: _$s15Synchronization19AtomicLazyReferenceV4loadxSgyF

// Synchronization.AtomicLazyReference.storage.read : Synchronization.Atomic<Swift.Unmanaged<A>?>
Added: _$s15Synchronization19AtomicLazyReferenceV7storageAA0B0Vys9UnmanagedVyxGSgGvr

// Synchronization.AtomicLazyReference.init() -> Synchronization.AtomicLazyReference<A>
Added: _$s15Synchronization19AtomicLazyReferenceVACyxGycfC

// type metadata accessor for Synchronization.AtomicLazyReference
Added: _$s15Synchronization19AtomicLazyReferenceVMa

// nominal type descriptor for Synchronization.AtomicLazyReference
Added: _$s15Synchronization19AtomicLazyReferenceVMn

// Synchronization.AtomicLazyReference.deinit
Added: _$s15Synchronization19AtomicLazyReferenceVfD

// Synchronization.AtomicStoreOrdering.description.getter : Swift.String
Added: _$s15Synchronization19AtomicStoreOrderingV11descriptionSSvg

// property descriptor for Synchronization.AtomicStoreOrdering.description : Swift.String
Added: _$s15Synchronization19AtomicStoreOrderingV11descriptionSSvpMV

// static Synchronization.AtomicStoreOrdering.== infix(Synchronization.AtomicStoreOrdering, Synchronization.AtomicStoreOrdering) -> Swift.Bool
Added: _$s15Synchronization19AtomicStoreOrderingV2eeoiySbAC_ACtFZ

// Synchronization.AtomicStoreOrdering.hash(into: inout Swift.Hasher) -> ()
Added: _$s15Synchronization19AtomicStoreOrderingV4hash4intoys6HasherVz_tF

// Synchronization.AtomicStoreOrdering.init(_rawValue: Swift.Int) -> Synchronization.AtomicStoreOrdering
Added: _$s15Synchronization19AtomicStoreOrderingV9_rawValueACSi_tcfC

// Synchronization.AtomicStoreOrdering._rawValue.modify : Swift.Int
Added: _$s15Synchronization19AtomicStoreOrderingV9_rawValueSivM

// Synchronization.AtomicStoreOrdering._rawValue.getter : Swift.Int
Added: _$s15Synchronization19AtomicStoreOrderingV9_rawValueSivg

// property descriptor for Synchronization.AtomicStoreOrdering._rawValue : Swift.Int
Added: _$s15Synchronization19AtomicStoreOrderingV9_rawValueSivpMV

// Synchronization.AtomicStoreOrdering._rawValue.setter : Swift.Int
Added: _$s15Synchronization19AtomicStoreOrderingV9_rawValueSivs

// Synchronization.AtomicStoreOrdering.hashValue.getter : Swift.Int
Added: _$s15Synchronization19AtomicStoreOrderingV9hashValueSivg

// property descriptor for Synchronization.AtomicStoreOrdering.hashValue : Swift.Int
Added: _$s15Synchronization19AtomicStoreOrderingV9hashValueSivpMV

// type metadata accessor for Synchronization.AtomicStoreOrdering
Added: _$s15Synchronization19AtomicStoreOrderingVMa

// nominal type descriptor for Synchronization.AtomicStoreOrdering
Added: _$s15Synchronization19AtomicStoreOrderingVMn

// type metadata for Synchronization.AtomicStoreOrdering
Added: _$s15Synchronization19AtomicStoreOrderingVN

// protocol conformance descriptor for Synchronization.AtomicStoreOrdering : Swift.Hashable in Synchronization
Added: _$s15Synchronization19AtomicStoreOrderingVSHAAMc

// protocol conformance descriptor for Synchronization.AtomicStoreOrdering : Swift.Equatable in Synchronization
Added: _$s15Synchronization19AtomicStoreOrderingVSQAAMc

// protocol conformance descriptor for Synchronization.AtomicStoreOrdering : Swift.CustomStringConvertible in Synchronization
Added: _$s15Synchronization19AtomicStoreOrderingVs23CustomStringConvertibleAAMc

// Synchronization._Atomic16BitStorage._storage.modify : Builtin.Int16
Added: _$s15Synchronization19_Atomic16BitStorageV8_storageBi16_vM

// Synchronization._Atomic16BitStorage._storage.getter : Builtin.Int16
Added: _$s15Synchronization19_Atomic16BitStorageV8_storageBi16_vg

// property descriptor for Synchronization._Atomic16BitStorage._storage : Builtin.Int16
Added: _$s15Synchronization19_Atomic16BitStorageV8_storageBi16_vpMV

// Synchronization._Atomic16BitStorage._storage.setter : Builtin.Int16
Added: _$s15Synchronization19_Atomic16BitStorageV8_storageBi16_vs

// type metadata accessor for Synchronization._Atomic16BitStorage
Added: _$s15Synchronization19_Atomic16BitStorageVMa

// nominal type descriptor for Synchronization._Atomic16BitStorage
Added: _$s15Synchronization19_Atomic16BitStorageVMn

// type metadata for Synchronization._Atomic16BitStorage
Added: _$s15Synchronization19_Atomic16BitStorageVN

// Synchronization._Atomic32BitStorage._storage.modify : Builtin.Int32
Added: _$s15Synchronization19_Atomic32BitStorageV8_storageBi32_vM

// Synchronization._Atomic32BitStorage._storage.getter : Builtin.Int32
Added: _$s15Synchronization19_Atomic32BitStorageV8_storageBi32_vg

// property descriptor for Synchronization._Atomic32BitStorage._storage : Builtin.Int32
Added: _$s15Synchronization19_Atomic32BitStorageV8_storageBi32_vpMV

// Synchronization._Atomic32BitStorage._storage.setter : Builtin.Int32
Added: _$s15Synchronization19_Atomic32BitStorageV8_storageBi32_vs

// type metadata accessor for Synchronization._Atomic32BitStorage
Added: _$s15Synchronization19_Atomic32BitStorageVMa

// nominal type descriptor for Synchronization._Atomic32BitStorage
Added: _$s15Synchronization19_Atomic32BitStorageVMn

// type metadata for Synchronization._Atomic32BitStorage
Added: _$s15Synchronization19_Atomic32BitStorageVN

// Synchronization._Atomic64BitStorage._storage.modify : Builtin.Int64
Added: _$s15Synchronization19_Atomic64BitStorageV8_storageBi64_vM

// Synchronization._Atomic64BitStorage._storage.getter : Builtin.Int64
Added: _$s15Synchronization19_Atomic64BitStorageV8_storageBi64_vg

// property descriptor for Synchronization._Atomic64BitStorage._storage : Builtin.Int64
Added: _$s15Synchronization19_Atomic64BitStorageV8_storageBi64_vpMV

// Synchronization._Atomic64BitStorage._storage.setter : Builtin.Int64
Added: _$s15Synchronization19_Atomic64BitStorageV8_storageBi64_vs

// type metadata accessor for Synchronization._Atomic64BitStorage
Added: _$s15Synchronization19_Atomic64BitStorageVMa

// nominal type descriptor for Synchronization._Atomic64BitStorage
Added: _$s15Synchronization19_Atomic64BitStorageVMn

// type metadata for Synchronization._Atomic64BitStorage
Added: _$s15Synchronization19_Atomic64BitStorageVN

// Synchronization.AtomicUpdateOrdering.description.getter : Swift.String
Added: _$s15Synchronization20AtomicUpdateOrderingV11descriptionSSvg

// property descriptor for Synchronization.AtomicUpdateOrdering.description : Swift.String
Added: _$s15Synchronization20AtomicUpdateOrderingV11descriptionSSvpMV

// static Synchronization.AtomicUpdateOrdering.== infix(Synchronization.AtomicUpdateOrdering, Synchronization.AtomicUpdateOrdering) -> Swift.Bool
Added: _$s15Synchronization20AtomicUpdateOrderingV2eeoiySbAC_ACtFZ

// Synchronization.AtomicUpdateOrdering.hash(into: inout Swift.Hasher) -> ()
Added: _$s15Synchronization20AtomicUpdateOrderingV4hash4intoys6HasherVz_tF

// Synchronization.AtomicUpdateOrdering.init(_rawValue: Swift.Int) -> Synchronization.AtomicUpdateOrdering
Added: _$s15Synchronization20AtomicUpdateOrderingV9_rawValueACSi_tcfC

// Synchronization.AtomicUpdateOrdering._rawValue.modify : Swift.Int
Added: _$s15Synchronization20AtomicUpdateOrderingV9_rawValueSivM

// Synchronization.AtomicUpdateOrdering._rawValue.getter : Swift.Int
Added: _$s15Synchronization20AtomicUpdateOrderingV9_rawValueSivg

// property descriptor for Synchronization.AtomicUpdateOrdering._rawValue : Swift.Int
Added: _$s15Synchronization20AtomicUpdateOrderingV9_rawValueSivpMV

// Synchronization.AtomicUpdateOrdering._rawValue.setter : Swift.Int
Added: _$s15Synchronization20AtomicUpdateOrderingV9_rawValueSivs

// Synchronization.AtomicUpdateOrdering.hashValue.getter : Swift.Int
Added: _$s15Synchronization20AtomicUpdateOrderingV9hashValueSivg

// property descriptor for Synchronization.AtomicUpdateOrdering.hashValue : Swift.Int
Added: _$s15Synchronization20AtomicUpdateOrderingV9hashValueSivpMV

// type metadata accessor for Synchronization.AtomicUpdateOrdering
Added: _$s15Synchronization20AtomicUpdateOrderingVMa

// nominal type descriptor for Synchronization.AtomicUpdateOrdering
Added: _$s15Synchronization20AtomicUpdateOrderingVMn

// type metadata for Synchronization.AtomicUpdateOrdering
Added: _$s15Synchronization20AtomicUpdateOrderingVN

// protocol conformance descriptor for Synchronization.AtomicUpdateOrdering : Swift.Hashable in Synchronization
Added: _$s15Synchronization20AtomicUpdateOrderingVSHAAMc

// protocol conformance descriptor for Synchronization.AtomicUpdateOrdering : Swift.Equatable in Synchronization
Added: _$s15Synchronization20AtomicUpdateOrderingVSQAAMc

// protocol conformance descriptor for Synchronization.AtomicUpdateOrdering : Swift.CustomStringConvertible in Synchronization
Added: _$s15Synchronization20AtomicUpdateOrderingVs23CustomStringConvertibleAAMc

// Synchronization._Atomic128BitStorage._storage.modify : Builtin.Int128
Added: _$s15Synchronization20_Atomic128BitStorageV8_storageBi128_vM

// Synchronization._Atomic128BitStorage._storage.getter : Builtin.Int128
Added: _$s15Synchronization20_Atomic128BitStorageV8_storageBi128_vg

// property descriptor for Synchronization._Atomic128BitStorage._storage : Builtin.Int128
Added: _$s15Synchronization20_Atomic128BitStorageV8_storageBi128_vpMV

// Synchronization._Atomic128BitStorage._storage.setter : Builtin.Int128
Added: _$s15Synchronization20_Atomic128BitStorageV8_storageBi128_vs

// type metadata accessor for Synchronization._Atomic128BitStorage
Added: _$s15Synchronization20_Atomic128BitStorageVMa

// nominal type descriptor for Synchronization._Atomic128BitStorage
Added: _$s15Synchronization20_Atomic128BitStorageVMn

// type metadata for Synchronization._Atomic128BitStorage
Added: _$s15Synchronization20_Atomic128BitStorageVN

// protocol descriptor for Synchronization.AtomicOptionalWrappable
Added: _$s15Synchronization23AtomicOptionalWrappableMp

// dispatch thunk of static Synchronization.AtomicOptionalWrappable.decodeAtomicOptionalRepresentation(__owned A.AtomicOptionalRepresentation) -> A?
Added: _$s15Synchronization23AtomicOptionalWrappableP06decodebC14RepresentationyxSg0bcF0QznFZTj

// method descriptor for static Synchronization.AtomicOptionalWrappable.decodeAtomicOptionalRepresentation(__owned A.AtomicOptionalRepresentation) -> A?
Added: _$s15Synchronization23AtomicOptionalWrappableP06decodebC14RepresentationyxSg0bcF0QznFZTq

// dispatch thunk of static Synchronization.AtomicOptionalWrappable.encodeAtomicOptionalRepresentation(__owned A?) -> A.AtomicOptionalRepresentation
Added: _$s15Synchronization23AtomicOptionalWrappableP06encodebC14Representationy0bcF0QzxSgnFZTj

// method descriptor for static Synchronization.AtomicOptionalWrappable.encodeAtomicOptionalRepresentation(__owned A?) -> A.AtomicOptionalRepresentation
Added: _$s15Synchronization23AtomicOptionalWrappableP06encodebC14Representationy0bcF0QzxSgnFZTq

// base conformance descriptor for Synchronization.AtomicOptionalWrappable: Synchronization.AtomicValue
Added: _$s15Synchronization23AtomicOptionalWrappablePAA0B5ValueTb

// protocol requirements base descriptor for Synchronization.AtomicOptionalWrappable
Added: _$s15Synchronization23AtomicOptionalWrappableTL

// type metadata accessor for Synchronization.Atomic
Added: _$s15Synchronization6AtomicVMa

// nominal type descriptor for Synchronization.Atomic
Added: _$s15Synchronization6AtomicVMn

// Synchronization.WordPair.description.getter : Swift.String
Added: _$s15Synchronization8WordPairV11descriptionSSvg

// property descriptor for Synchronization.WordPair.description : Swift.String
Added: _$s15Synchronization8WordPairV11descriptionSSvpMV

// Synchronization.WordPair.debugDescription.getter : Swift.String
Added: _$s15Synchronization8WordPairV16debugDescriptionSSvg

// property descriptor for Synchronization.WordPair.debugDescription : Swift.String
Added: _$s15Synchronization8WordPairV16debugDescriptionSSvpMV

//  Synchronization.WordPair.first.modify : Swift.UInt
Added: _$s15Synchronization8WordPairV5firstSuvM

// Synchronization.WordPair.first.getter : Swift.UInt
Added: _$s15Synchronization8WordPairV5firstSuvg

// property descriptor for Synchronization.WordPair.first : Swift.UInt
Added: _$s15Synchronization8WordPairV5firstSuvpMV

// Synchronization.WordPair.first.setter : Swift.UInt
Added: _$s15Synchronization8WordPairV5firstSuvs

// Synchronization.WordPair.second.modify : Swift.UInt
Added: _$s15Synchronization8WordPairV6secondSuvM

// Synchronization.WordPair.second.getter : Swift.UInt
Added: _$s15Synchronization8WordPairV6secondSuvg

// property descriptor for Synchronization.WordPair.second : Swift.UInt
Added: _$s15Synchronization8WordPairV6secondSuvpMV

// Synchronization.WordPair.second.setter : Swift.UInt
Added: _$s15Synchronization8WordPairV6secondSuvs

// Synchronization.WordPair.hashValue.getter : Swift.Int
Added: _$s15Synchronization8WordPairV9hashValueSivg

// property descriptor for Synchronization.WordPair.hashValue : Swift.Int
Added: _$s15Synchronization8WordPairV9hashValueSivpMV

// protocol conformance descriptor for Synchronization.WordPair : Synchronization.AtomicValue in Synchronization
Added: _$s15Synchronization8WordPairVAA11AtomicValueAAMc

// protocol witness table for Synchronization.WordPair : Synchronization.AtomicValue in Synchronization
Added: _$s15Synchronization8WordPairVAA11AtomicValueAAWP

// type metadata accessor for Synchronization.WordPair
Added: _$s15Synchronization8WordPairVMa

// nominal type descriptor for Synchronization.WordPair
Added: _$s15Synchronization8WordPairVMn

// type metadata for Synchronization.WordPair
Added: _$s15Synchronization8WordPairVN

// protocol conformance descriptor for Synchronization.WordPair : Swift.Hashable in Synchronization
Added: _$s15Synchronization8WordPairVSHAAMc

// protocol conformance descriptor for Synchronization.WordPair : Swift.Equatable in Synchronization
Added: _$s15Synchronization8WordPairVSQAAMc

// protocol conformance descriptor for Synchronization.WordPair : Swift.CustomStringConvertible in Synchronization
Added: _$s15Synchronization8WordPairVs23CustomStringConvertibleAAMc

// protocol conformance descriptor for Synchronization.WordPair : Swift.CustomDebugStringConvertible in Synchronization
Added: _$s15Synchronization8WordPairVs28CustomDebugStringConvertibleAAMc

// associated type descriptor for Synchronization.AtomicValue.AtomicRepresentation
Added: _$s20AtomicRepresentation15Synchronization0A5ValuePTl

// associated type descriptor for Synchronization.AtomicOptionalWrappable.AtomicOptionalRepresentation
Added: _$s28AtomicOptionalRepresentation15Synchronization0aB9WrappablePTl

// protocol conformance descriptor for Swift.ObjectIdentifier : Synchronization.AtomicValue in Synchronization
Added: _$sSO15Synchronization11AtomicValueAAMc

// protocol witness table for Swift.ObjectIdentifier : Synchronization.AtomicValue in Synchronization
Added: _$sSO15Synchronization11AtomicValueAAWP

// protocol conformance descriptor for Swift.ObjectIdentifier : Synchronization.AtomicOptionalWrappable in Synchronization
Added: _$sSO15Synchronization23AtomicOptionalWrappableAAMc

// protocol witness table for Swift.ObjectIdentifier : Synchronization.AtomicOptionalWrappable in Synchronization
Added: _$sSO15Synchronization23AtomicOptionalWrappableAAWP

// protocol conformance descriptor for Swift.UnsafePointer<A> : Synchronization.AtomicValue in Synchronization
Added: _$sSPyxG15Synchronization11AtomicValueABMc

// protocol witness table for Swift.UnsafePointer<A> : Synchronization.AtomicValue in Synchronization
Added: _$sSPyxG15Synchronization11AtomicValueABWP

// protocol conformance descriptor for Swift.UnsafePointer<A> : Synchronization.AtomicOptionalWrappable in Synchronization
Added: _$sSPyxG15Synchronization23AtomicOptionalWrappableABMc

// protocol witness table for Swift.UnsafePointer<A> : Synchronization.AtomicOptionalWrappable in Synchronization
Added: _$sSPyxG15Synchronization23AtomicOptionalWrappableABWP

// protocol conformance descriptor for Swift.UnsafeBufferPointer<A> : Synchronization.AtomicValue in Synchronization
Added: _$sSRyxG15Synchronization11AtomicValueABMc

// protocol witness table for Swift.UnsafeBufferPointer<A> : Synchronization.AtomicValue in Synchronization
Added: _$sSRyxG15Synchronization11AtomicValueABWP

// protocol conformance descriptor for Swift.UnsafeRawPointer : Synchronization.AtomicValue in Synchronization
Added: _$sSV15Synchronization11AtomicValueAAMc

// protocol witness table for Swift.UnsafeRawPointer : Synchronization.AtomicValue in Synchronization
Added: _$sSV15Synchronization11AtomicValueAAWP

// protocol conformance descriptor for Swift.UnsafeRawPointer : Synchronization.AtomicOptionalWrappable in Synchronization
Added: _$sSV15Synchronization23AtomicOptionalWrappableAAMc

// protocol witness table for Swift.UnsafeRawPointer : Synchronization.AtomicOptionalWrappable in Synchronization
Added: _$sSV15Synchronization23AtomicOptionalWrappableAAWP

// protocol conformance descriptor for Swift.UnsafeRawBufferPointer : Synchronization.AtomicValue in Synchronization
Added: _$sSW15Synchronization11AtomicValueAAMc

// protocol witness table for Swift.UnsafeRawBufferPointer : Synchronization.AtomicValue in Synchronization
Added: _$sSW15Synchronization11AtomicValueAAWP

// protocol conformance descriptor for Swift.Bool : Synchronization.AtomicValue in Synchronization
Added: _$sSb15Synchronization11AtomicValueAAMc

// protocol witness table for Swift.Bool : Synchronization.AtomicValue in Synchronization
Added: _$sSb15Synchronization11AtomicValueAAWP

// protocol conformance descriptor for Swift.Double : Synchronization.AtomicValue in Synchronization
Added: _$sSd15Synchronization11AtomicValueAAMc

// protocol witness table for Swift.Double : Synchronization.AtomicValue in Synchronization
Added: _$sSd15Synchronization11AtomicValueAAWP

// protocol conformance descriptor for Swift.Float : Synchronization.AtomicValue in Synchronization
Added: _$sSf15Synchronization11AtomicValueAAMc

// protocol witness table for Swift.Float : Synchronization.AtomicValue in Synchronization
Added: _$sSf15Synchronization11AtomicValueAAWP

// protocol conformance descriptor for Swift.Int : Synchronization.AtomicValue in Synchronization
Added: _$sSi15Synchronization11AtomicValueAAMc

// protocol witness table for Swift.Int : Synchronization.AtomicValue in Synchronization
Added: _$sSi15Synchronization11AtomicValueAAWP

// protocol conformance descriptor for Swift.UnsafeMutablePointer<A> : Synchronization.AtomicValue in Synchronization
Added: _$sSpyxG15Synchronization11AtomicValueABMc

// protocol witness table for Swift.UnsafeMutablePointer<A> : Synchronization.AtomicValue in Synchronization
Added: _$sSpyxG15Synchronization11AtomicValueABWP

// protocol conformance descriptor for Swift.UnsafeMutablePointer<A> : Synchronization.AtomicOptionalWrappable in Synchronization
Added: _$sSpyxG15Synchronization23AtomicOptionalWrappableABMc

// protocol witness table for Swift.UnsafeMutablePointer<A> : Synchronization.AtomicOptionalWrappable in Synchronization
Added: _$sSpyxG15Synchronization23AtomicOptionalWrappableABWP

// protocol conformance descriptor for Swift.UnsafeMutableBufferPointer<A> : Synchronization.AtomicValue in Synchronization
Added: _$sSryxG15Synchronization11AtomicValueABMc

// protocol witness table for Swift.UnsafeMutableBufferPointer<A> : Synchronization.AtomicValue in Synchronization
Added: _$sSryxG15Synchronization11AtomicValueABWP

// protocol conformance descriptor for Swift.UInt : Synchronization.AtomicValue in Synchronization
Added: _$sSu15Synchronization11AtomicValueAAMc

// protocol witness table for Swift.UInt : Synchronization.AtomicValue in Synchronization
Added: _$sSu15Synchronization11AtomicValueAAWP

// protocol conformance descriptor for Swift.UnsafeMutableRawPointer : Synchronization.AtomicValue in Synchronization
Added: _$sSv15Synchronization11AtomicValueAAMc

// protocol witness table for Swift.UnsafeMutableRawPointer : Synchronization.AtomicValue in Synchronization
Added: _$sSv15Synchronization11AtomicValueAAWP

// protocol conformance descriptor for Swift.UnsafeMutableRawPointer : Synchronization.AtomicOptionalWrappable in Synchronization
Added: _$sSv15Synchronization23AtomicOptionalWrappableAAMc

// protocol witness table for Swift.UnsafeMutableRawPointer : Synchronization.AtomicOptionalWrappable in Synchronization
Added: _$sSv15Synchronization23AtomicOptionalWrappableAAWP

// protocol conformance descriptor for Swift.UnsafeMutableRawBufferPointer : Synchronization.AtomicValue in Synchronization
Added: _$sSw15Synchronization11AtomicValueAAMc

// protocol witness table for Swift.UnsafeMutableRawBufferPointer : Synchronization.AtomicValue in Synchronization
Added: _$sSw15Synchronization11AtomicValueAAWP

// protocol conformance descriptor for Swift.OpaquePointer : Synchronization.AtomicValue in Synchronization
Added: _$ss13OpaquePointerV15Synchronization11AtomicValueACMc

// protocol witness table for Swift.OpaquePointer : Synchronization.AtomicValue in Synchronization
Added: _$ss13OpaquePointerV15Synchronization11AtomicValueACWP

// protocol conformance descriptor for Swift.OpaquePointer : Synchronization.AtomicOptionalWrappable in Synchronization
Added: _$ss13OpaquePointerV15Synchronization23AtomicOptionalWrappableACMc

// protocol witness table for Swift.OpaquePointer : Synchronization.AtomicOptionalWrappable in Synchronization
Added: _$ss13OpaquePointerV15Synchronization23AtomicOptionalWrappableACWP

// protocol conformance descriptor for Swift.Int8 : Synchronization.AtomicValue in Synchronization
Added: _$ss4Int8V15Synchronization11AtomicValueACMc

// protocol witness table for Swift.Int8 : Synchronization.AtomicValue in Synchronization
Added: _$ss4Int8V15Synchronization11AtomicValueACWP

// protocol conformance descriptor for Swift.Int16 : Synchronization.AtomicValue in Synchronization
Added: _$ss5Int16V15Synchronization11AtomicValueACMc

// protocol witness table for Swift.Int16 : Synchronization.AtomicValue in Synchronization
Added: _$ss5Int16V15Synchronization11AtomicValueACWP

// protocol conformance descriptor for Swift.Int32 : Synchronization.AtomicValue in Synchronization
Added: _$ss5Int32V15Synchronization11AtomicValueACMc

// protocol witness table for Swift.Int32 : Synchronization.AtomicValue in Synchronization
Added: _$ss5Int32V15Synchronization11AtomicValueACWP

// protocol conformance descriptor for Swift.Int64 : Synchronization.AtomicValue in Synchronization
Added: _$ss5Int64V15Synchronization11AtomicValueACMc

// protocol witness table for Swift.Int64 : Synchronization.AtomicValue in Synchronization
Added: _$ss5Int64V15Synchronization11AtomicValueACWP

// protocol conformance descriptor for Swift.Never : Synchronization.AtomicValue in Synchronization
Added: _$ss5NeverO15Synchronization11AtomicValueACMc

// protocol witness table for Swift.Never : Synchronization.AtomicValue in Synchronization
Added: _$ss5NeverO15Synchronization11AtomicValueACWP

// protocol conformance descriptor for Swift.UInt8 : Synchronization.AtomicValue in Synchronization
Added: _$ss5UInt8V15Synchronization11AtomicValueACMc

// protocol witness table for Swift.UInt8 : Synchronization.AtomicValue in Synchronization
Added: _$ss5UInt8V15Synchronization11AtomicValueACWP

// protocol conformance descriptor for Swift.UInt16 : Synchronization.AtomicValue in Synchronization
Added: _$ss6UInt16V15Synchronization11AtomicValueACMc

// protocol witness table for Swift.UInt16 : Synchronization.AtomicValue in Synchronization
Added: _$ss6UInt16V15Synchronization11AtomicValueACWP

// protocol conformance descriptor for Swift.UInt32 : Synchronization.AtomicValue in Synchronization
Added: _$ss6UInt32V15Synchronization11AtomicValueACMc

// protocol witness table for Swift.UInt32 : Synchronization.AtomicValue in Synchronization
Added: _$ss6UInt32V15Synchronization11AtomicValueACWP

// protocol conformance descriptor for Swift.UInt64 : Synchronization.AtomicValue in Synchronization
Added: _$ss6UInt64V15Synchronization11AtomicValueACMc

// protocol witness table for Swift.UInt64 : Synchronization.AtomicValue in Synchronization
Added: _$ss6UInt64V15Synchronization11AtomicValueACWP

// protocol conformance descriptor for Swift.Float16 : Synchronization.AtomicValue in Synchronization
Added: _$ss7Float16V15Synchronization11AtomicValueACMc

// protocol witness table for Swift.Float16 : Synchronization.AtomicValue in Synchronization
Added: _$ss7Float16V15Synchronization11AtomicValueACWP

// protocol conformance descriptor for Swift.Duration : Synchronization.AtomicValue in Synchronization
Added: _$ss8DurationV15Synchronization11AtomicValueACMc

// protocol witness table for Swift.Duration : Synchronization.AtomicValue in Synchronization
Added: _$ss8DurationV15Synchronization11AtomicValueACWP

// protocol conformance descriptor for Swift.Unmanaged<A> : Synchronization.AtomicValue in Synchronization
Added: _$ss9UnmanagedVyxG15Synchronization11AtomicValueADMc

// protocol witness table for Swift.Unmanaged<A> : Synchronization.AtomicValue in Synchronization
Added: _$ss9UnmanagedVyxG15Synchronization11AtomicValueADWP

// protocol conformance descriptor for Swift.Unmanaged<A> : Synchronization.AtomicOptionalWrappable in Synchronization
Added: _$ss9UnmanagedVyxG15Synchronization23AtomicOptionalWrappableADMc

// protocol witness table for Swift.Unmanaged<A> : Synchronization.AtomicOptionalWrappable in Synchronization
Added: _$ss9UnmanagedVyxG15Synchronization23AtomicOptionalWrappableADWP

// protocol conformance descriptor for <A where A: Synchronization.AtomicOptionalWrappable> A? : Synchronization.AtomicValue in Synchronization
Added: _$sxSg15Synchronization11AtomicValueA2B0B17OptionalWrappableRzlMc
