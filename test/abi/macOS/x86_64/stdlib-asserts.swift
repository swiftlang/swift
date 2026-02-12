// RUN: %empty-directory(%t)
// RUN: %llvm-nm -g --defined-only -f just-symbols %stdlib_dir/x86_64/libswiftCore.dylib > %t/symbols
// RUN: %abi-symbol-checker %s %t/symbols --base %S/stdlib.swift
// RUN: diff -u %S/../../Inputs/macOS/x86_64/stdlib/baseline-asserts %t/symbols

// REQUIRES: swift_stdlib_asserts
// REQUIRES: STDLIB_VARIANT=macosx-x86_64

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

// *** NOTE: ***
// You will normally add new entries in 'abi/macOS/x86_64/stdlib.swift' instead
// of this file. This file is dedicated for assert only symbols.

// Standard Library Symbols

// class __StaticArrayStorage
Added: _$ss20__StaticArrayStorageC12_doNotCallMeAByt_tcfC
Added: _$ss20__StaticArrayStorageC12_doNotCallMeAByt_tcfCTj
Added: _$ss20__StaticArrayStorageC12_doNotCallMeAByt_tcfCTq
Added: _$ss20__StaticArrayStorageC12_doNotCallMeAByt_tcfc
Added: _$ss20__StaticArrayStorageC16_doNotCallMeBaseAByt_tcfC
Added: _$ss20__StaticArrayStorageC16_doNotCallMeBaseAByt_tcfc
Added: _$ss20__StaticArrayStorageC16canStoreElements13ofDynamicTypeSbypXp_tF
Added: _$ss20__StaticArrayStorageC17staticElementTypeypXpvg
Added: _$ss20__StaticArrayStorageCMa
Added: _$ss20__StaticArrayStorageCMn
Added: _$ss20__StaticArrayStorageCMo
Added: _$ss20__StaticArrayStorageCMu
Added: _$ss20__StaticArrayStorageCN
Added: _$ss20__StaticArrayStorageCfD
Added: _$ss20__StaticArrayStorageCfd
Added: _OBJC_CLASS_$__TtCs20__StaticArrayStorage
Added: _OBJC_METACLASS_$__TtCs20__StaticArrayStorage

Added: _$ss24_RuntimeFunctionCountersV03numabC0SivpZMV
Added: _$ss24_RuntimeFunctionCountersV07runtimeB11NameToIndexSDySSSiGvpZMV
Added: _$ss24_RuntimeFunctionCountersV07runtimeB5NamesSaySSGvpZMV
Added: _$ss24_RuntimeFunctionCountersV07runtimebC7OffsetsSPys6UInt16VGvpZMV

// Runtime Symbols
Added: _swift_clearSensitive

// BorrowingSequence + supporting symbols
Added: _$s18_BorrowingIterators01_A8SequencePTl
Added: _$s8_Elements18_BorrowingSequencePTl
Added: _$s8_Elements26_BorrowingIteratorProtocolPTl
Added: _$ss11InlineArrayVsRi__rlE22_makeBorrowingIterators4SpanVyq_GyF
Added: _$ss11InlineArrayVyxq_Gs18_BorrowingSequencesRi__rlMc
Added: _$ss18_BorrowingSequenceMp
Added: _$ss18_BorrowingSequenceP01_A8IteratorAB_s01_aC8ProtocolTn
Added: _$ss18_BorrowingSequenceP05_makeA8Iterator01_aD0QzyFTj
Added: _$ss18_BorrowingSequenceP05_makeA8Iterator01_aD0QzyFTq
Added: _$ss18_BorrowingSequencePs01_A8IteratorABQzRszRi0_zrlE04makeaC0xyF
Added: _$ss18_BorrowingSequenceTL
Added: _$ss26_BorrowingIteratorProtocolMp
Added: _$ss26_BorrowingIteratorProtocolP5_skip2byS2i_tFTj
Added: _$ss26_BorrowingIteratorProtocolP5_skip2byS2i_tFTq
Added: _$ss26_BorrowingIteratorProtocolP9_nextSpan12maximumCounts0E0Vy8_ElementQzGSi_tFTj
Added: _$ss26_BorrowingIteratorProtocolP9_nextSpan12maximumCounts0E0Vy8_ElementQzGSi_tFTq
Added: _$ss26_BorrowingIteratorProtocolTL
Added: _$ss4SpanVsRi_zrlE05_nextA012maximumCountAByxGSi_tF
Added: _$ss4SpanVsRi_zrlE22_makeBorrowingIteratorAByxGyF
Added: _$ss4SpanVyxGs18_BorrowingSequencesRi_zrlMc
Added: _$ss4SpanVyxGs26_BorrowingIteratorProtocolsRi_zrlMc
