// RUN: %empty-directory(%t)
// RUN: %llvm-nm -g --defined-only -f just-symbols %stdlib_dir/x86_64/libswiftCore.dylib > %t/symbols
// RUN: %abi-symbol-checker %s %t/symbols
// RUN: diff -u %S/../../Inputs/macOS/x86_64/stdlib/baseline %t/symbols

// REQUIRES: swift_stdlib_no_asserts
// REQUIRES: STDLIB_VARIANT=macosx-x86_64
// REQUIRES: backtracing

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
// Swift Symbols
//===----------------------------------------------------------------------===//

// String.init<Encoding: Unicode.Encoding>(_immortalCocoaString: AnyObject, count: Int, encoding: Encoding.Type)
Added: _$sSS20_immortalCocoaString5count8encodingSSyXl_Sixmtcs16_UnicodeEncodingRzlufC

// Swift._stdlib_isVariantOSVersionAtLeast(Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
Added: _$ss33_stdlib_isVariantOSVersionAtLeastyBi1_Bw_BwBwtF

// Typed throws for withUnsafeBufferPointer operations.
Added: _$ss20_ArrayBufferProtocolP010withUnsafeB7Pointeryqd__qd__SRy7ElementQzGqd_0_YKXEqd_0_YKs5ErrorRd_0_r0_lFTj
Added: _$ss20_ArrayBufferProtocolP010withUnsafeB7Pointeryqd__qd__SRy7ElementQzGqd_0_YKXEqd_0_YKs5ErrorRd_0_r0_lFTq

// Typed throws for withUnsafeMutableBufferPointer operations.
Added: _$ss20_ArrayBufferProtocolP017withUnsafeMutableB7Pointeryqd__qd__Sry7ElementQzGqd_0_YKXEqd_0_YKs5ErrorRd_0_r0_lFTj
Added: _$ss20_ArrayBufferProtocolP017withUnsafeMutableB7Pointeryqd__qd__Sry7ElementQzGqd_0_YKXEqd_0_YKs5ErrorRd_0_r0_lFTq

// Add property descriptors for static properties
Added: _$sSP4_maxSPyxGvpZMV
Added: _$sSS5IndexV10__utf16Bits6UInt64VvpZMV
Added: _$sSS5IndexV20__scalarAlignmentBits6UInt64VvpZMV
Added: _$sSS5IndexV23__characterAlignmentBits6UInt64VvpZMV
Added: _$sSS5IndexV9__utf8Bits6UInt64VvpZMV
Added: _$sSd12signalingNaNSdvpZMV
Added: _$sSd13_exponentBiasSuvpZMV
Added: _$sSd13_quietNaNMasks6UInt64VvpZMV
Added: _$sSd16_significandMasks6UInt64VvpZMV
Added: _$sSd16exponentBitCountSivpZMV
Added: _$sSd17_infinityExponentSuvpZMV
Added: _$sSd19significandBitCountSivpZMV
Added: _$sSd20leastNormalMagnitudeSdvpZMV
Added: _$sSd21leastNonzeroMagnitudeSdvpZMV
Added: _$sSd23greatestFiniteMagnitudeSdvpZMV
Added: _$sSd2piSdvpZMV
Added: _$sSd3nanSdvpZMV
Added: _$sSd8infinitySdvpZMV
Added: _$sSd8quietNaNSdvpZMV
Added: _$sSd8ulpOfOneSdvpZMV
Added: _$sSf12signalingNaNSfvpZMV
Added: _$sSf13_exponentBiasSuvpZMV
Added: _$sSf13_quietNaNMasks6UInt32VvpZMV
Added: _$sSf16_significandMasks6UInt32VvpZMV
Added: _$sSf16exponentBitCountSivpZMV
Added: _$sSf17_infinityExponentSuvpZMV
Added: _$sSf19significandBitCountSivpZMV
Added: _$sSf20leastNormalMagnitudeSfvpZMV
Added: _$sSf21leastNonzeroMagnitudeSfvpZMV
Added: _$sSf23greatestFiniteMagnitudeSfvpZMV
Added: _$sSf2piSfvpZMV
Added: _$sSf3nanSfvpZMV
Added: _$sSf8infinitySfvpZMV
Added: _$sSf8quietNaNSfvpZMV
Added: _$sSf8ulpOfOneSfvpZMV
Added: _$sSi8bitWidthSivpZMV
Added: _$sSo19_SwiftStdlibVersionasE6v5_6_0ABvpZMV
Added: _$sSo19_SwiftStdlibVersionasE6v5_7_0ABvpZMV
Added: _$sSo19_SwiftStdlibVersionasE6v5_8_0ABvpZMV
Added: _$sSo19_SwiftStdlibVersionasE6v5_9_0ABvpZMV
Added: _$sSo19_SwiftStdlibVersionasE6v6_0_0ABvpZMV
Added: _$sSo19_SwiftStdlibVersionasE7currentABvpZMV
Added: _$sSo19_SwiftStdlibVersionasE7v5_10_0ABvpZMV
Added: _$sSp4_maxSpyxGvpZMV
Added: _$sSu8bitWidthSivpZMV
Added: _$ss10AnyKeyPathC17_rootAndValueTypeypXp0D0_ypXp5valuetvpZMV
Added: _$ss10AnyKeyPathC8rootTypeypXpvpZMV
Added: _$ss10AnyKeyPathC9valueTypeypXpvpZMV
Added: _$ss11CommandLineO10unsafeArgvSpySpys4Int8VGSgGvpZMV
Added: _$ss11CommandLineO4argcs5Int32VvpZMV
Added: _$ss11CommandLineO9argumentsSaySSGvpZMV
Added: _$ss12MemoryLayoutO4sizeSivpZMV
Added: _$ss12MemoryLayoutO6strideSivpZMV
Added: _$ss12MemoryLayoutO9alignmentSivpZMV
Added: _$ss12_SmallStringV8capacitySivpZMV
Added: _$ss13_StringObjectV10nativeBiasSuvpZMV
Added: _$ss13_StringObjectV13CountAndFlagsV11isASCIIMasks6UInt64VvpZMV
Added: _$ss13_StringObjectV13CountAndFlagsV17isForeignUTF8Masks6UInt64VvpZMV
Added: _$ss13_StringObjectV13CountAndFlagsV19isTailAllocatedMasks6UInt64VvpZMV
Added: _$ss13_StringObjectV13CountAndFlagsV20isNativelyStoredMasks6UInt64VvpZMV
Added: _$ss13_StringObjectV13CountAndFlagsV9countMasks6UInt64VvpZMV
Added: _$ss13_StringObjectV13CountAndFlagsV9flagsMasks6UInt64VvpZMV
Added: _$ss13_StringObjectV13CountAndFlagsV9isNFCMasks6UInt64VvpZMV
Added: _$ss13_StringObjectV7NibblesO05emptyA0s6UInt64VvpZMV
Added: _$ss13_StringObjectV7NibblesO16largeAddressMasks6UInt64VvpZMV
Added: _$ss13_StringObjectV7NibblesO17discriminatorMasks6UInt64VvpZMV
Added: _$ss13_UnsafeBitsetV4WordV5emptyADvpZMV
Added: _$ss13_UnsafeBitsetV4WordV7allBitsADvpZMV
Added: _$ss13_UnsafeBitsetV4WordV8capacitySivpZMV
Added: _$ss14_BridgeStorageV8flagMaskSuvpZMV
Added: _$ss15__RawSetStorageC5emptys07__EmptyB9SingletonCvpZMV
Added: _$ss16_ValidUTF8BufferV27encodedReplacementCharacterABvpZMV
Added: _$ss16_ValidUTF8BufferV8capacitySivpZMV
Added: _$ss17_EachFieldOptionsV13ignoreUnknownABvpZMV
Added: _$ss17_EachFieldOptionsV9classTypeABvpZMV
Added: _$ss20ManagedBufferPointerV13_headerOffsetSivpZMV
Added: _$ss20ManagedBufferPointerV14_alignmentMaskSivpZMV
Added: _$ss20ManagedBufferPointerV14_elementOffsetSivpZMV
Added: _$ss22__RawDictionaryStorageC5emptys07__EmptyB9SingletonCvpZMV
Added: _$ss4Int8V8bitWidthSivpZMV
Added: _$ss5Int16V8bitWidthSivpZMV
Added: _$ss5Int32V8bitWidthSivpZMV
Added: _$ss5Int64V8bitWidthSivpZMV
Added: _$ss5UInt8V8bitWidthSivpZMV
Added: _$ss6HasherV14_executionSeeds6UInt64V_AEtvpZMV
Added: _$ss6HasherV16_isDeterministicSbvpZMV
Added: _$ss6Int128V3maxABvpZMV
Added: _$ss6Int128V3minABvpZMV
Added: _$ss6Int128V4zeroABvpZMV
Added: _$ss6Int128V8bitWidthSivpZMV
Added: _$ss6UInt16V8bitWidthSivpZMV
Added: _$ss6UInt32V8bitWidthSivpZMV
Added: _$ss6UInt64V8bitWidthSivpZMV
Added: _$ss7UInt128V3maxABvpZMV
Added: _$ss7UInt128V3minABvpZMV
Added: _$ss7UInt128V4zeroABvpZMV
Added: _$ss7UInt128V8bitWidthSivpZMV
Added: _$ss7UnicodeO23CanonicalCombiningClassV10aboveRightADvpZMV
Added: _$ss7UnicodeO23CanonicalCombiningClassV10belowRightADvpZMV
Added: _$ss7UnicodeO23CanonicalCombiningClassV11doubleAboveADvpZMV
Added: _$ss7UnicodeO23CanonicalCombiningClassV11doubleBelowADvpZMV
Added: _$ss7UnicodeO23CanonicalCombiningClassV11kanaVoicingADvpZMV
Added: _$ss7UnicodeO23CanonicalCombiningClassV12notReorderedADvpZMV
Added: _$ss7UnicodeO23CanonicalCombiningClassV13attachedAboveADvpZMV
Added: _$ss7UnicodeO23CanonicalCombiningClassV13attachedBelowADvpZMV
Added: _$ss7UnicodeO23CanonicalCombiningClassV13iotaSubscriptADvpZMV
Added: _$ss7UnicodeO23CanonicalCombiningClassV17attachedBelowLeftADvpZMV
Added: _$ss7UnicodeO23CanonicalCombiningClassV18attachedAboveRightADvpZMV
Added: _$ss7UnicodeO23CanonicalCombiningClassV4leftADvpZMV
Added: _$ss7UnicodeO23CanonicalCombiningClassV5aboveADvpZMV
Added: _$ss7UnicodeO23CanonicalCombiningClassV5belowADvpZMV
Added: _$ss7UnicodeO23CanonicalCombiningClassV5nuktaADvpZMV
Added: _$ss7UnicodeO23CanonicalCombiningClassV5rightADvpZMV
Added: _$ss7UnicodeO23CanonicalCombiningClassV6viramaADvpZMV
Added: _$ss7UnicodeO23CanonicalCombiningClassV7overlayADvpZMV
Added: _$ss7UnicodeO23CanonicalCombiningClassV9aboveLeftADvpZMV
Added: _$ss7UnicodeO23CanonicalCombiningClassV9belowLeftADvpZMV
Added: _$ss7UnicodeO4UTF8O27encodedReplacementCharacters06_ValidB6BufferVvpZMV
Added: _$ss7UnicodeO5UTF32O20_replacementCodeUnits6UInt32VvpZMV
Added: _$ss8DurationV4zeroABvpZMV
Added: _$ss8SIMDMaskVss5SIMD2VySiGRszrlE7allTrueAByAEGvpZMV
Added: _$ss8SIMDMaskVss5SIMD2Vys4Int8VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss5SIMD2Vys5Int16VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss5SIMD2Vys5Int32VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss5SIMD2Vys5Int64VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss5SIMD3VySiGRszrlE7allTrueAByAEGvpZMV
Added: _$ss8SIMDMaskVss5SIMD3Vys4Int8VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss5SIMD3Vys5Int16VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss5SIMD3Vys5Int32VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss5SIMD3Vys5Int64VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss5SIMD4VySiGRszrlE7allTrueAByAEGvpZMV
Added: _$ss8SIMDMaskVss5SIMD4Vys4Int8VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss5SIMD4Vys5Int16VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss5SIMD4Vys5Int32VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss5SIMD4Vys5Int64VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss5SIMD8VySiGRszrlE7allTrueAByAEGvpZMV
Added: _$ss8SIMDMaskVss5SIMD8Vys4Int8VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss5SIMD8Vys5Int16VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss5SIMD8Vys5Int32VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss5SIMD8Vys5Int64VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss6SIMD16VySiGRszrlE7allTrueAByAEGvpZMV
Added: _$ss8SIMDMaskVss6SIMD16Vys4Int8VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss6SIMD16Vys5Int16VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss6SIMD16Vys5Int32VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss6SIMD16Vys5Int64VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss6SIMD32VySiGRszrlE7allTrueAByAEGvpZMV
Added: _$ss8SIMDMaskVss6SIMD32Vys4Int8VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss6SIMD32Vys5Int16VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss6SIMD32Vys5Int32VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss6SIMD32Vys5Int64VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss6SIMD64VySiGRszrlE7allTrueAByAEGvpZMV
Added: _$ss8SIMDMaskVss6SIMD64Vys4Int8VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss6SIMD64Vys5Int16VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss6SIMD64Vys5Int32VGRszrlE7allTrueAByAGGvpZMV
Added: _$ss8SIMDMaskVss6SIMD64Vys5Int64VGRszrlE7allTrueAByAGGvpZMV
Added: _$sSo19_SwiftStdlibVersionasE6v6_1_0ABvpZMV
Added: _$ss7Float80V12signalingNaNABvpZMV
Added: _$ss7Float80V13_exponentBiasSuvpZMV
Added: _$ss7Float80V13_quietNaNMasks6UInt64VvpZMV
Added: _$ss7Float80V16_explicitBitMasks6UInt64VvpZMV
Added: _$ss7Float80V16_significandMasks6UInt64VvpZMV
Added: _$ss7Float80V16exponentBitCountSivpZMV
Added: _$ss7Float80V17_infinityExponentSuvpZMV
Added: _$ss7Float80V19significandBitCountSivpZMV
Added: _$ss7Float80V20leastNormalMagnitudeABvpZMV
Added: _$ss7Float80V21leastNonzeroMagnitudeABvpZMV
Added: _$ss7Float80V23greatestFiniteMagnitudeABvpZMV
Added: _$ss7Float80V2piABvpZMV
Added: _$ss7Float80V3nanABvpZMV
Added: _$ss7Float80V8infinityABvpZMV
Added: _$ss7Float80V8quietNaNABvpZMV
Added: _$ss7Float80V8ulpOfOneABvpZMV
Added: _$ss7UnicodeO5ASCIIO27encodedReplacementCharacters15CollectionOfOneVys5UInt8VGvpZMV
Added: _$ss7UnicodeO5UTF16O20_replacementCodeUnits6UInt16VvpZMV
Added: _$ss7UnicodeO5UTF16O27encodedReplacementCharacters11_UIntBufferVys6UInt16VGvpZMV
Added: _$ss7UnicodeO5UTF32O27encodedReplacementCharacters15CollectionOfOneVys6UInt32VGvpZMV

// SE-0445 Improving printed descriptions of String.Index
Added: _$sSS5IndexV16debugDescriptionSSvpMV
Added: _$sSS5IndexVs28CustomDebugStringConvertiblesMc
Added: _$sSS5IndexVs28CustomDebugStringConvertiblesWP

// SE-0447 Span and RawSpan
Added: _$ss4SpanVMa
Added: _$ss4SpanVMn
Added: _$ss4SpanVsRi_zRi0_zrlE6_countSivg
Added: _$ss4SpanVsRi_zRi0_zrlE6_countSivpMV
Added: _$ss4SpanVsRi_zRi0_zrlE8_pointerSVSgvg
Added: _$ss4SpanVsRi_zRi0_zrlE8_pointerSVSgvpMV
Added: _$ss4SpanVsRi_zrlE5countSivpMV
Added: _$ss4SpanVsRi_zrlE7indicesSnySiGvpMV
Added: _$ss4SpanVsRi_zrlE7isEmptySbvpMV
Added: _$ss4SpanVss15BitwiseCopyableRzlE9uncheckedxSi_tcipMV
Added: _$ss4SpanVss15BitwiseCopyableRzlEyxSicipMV
Added: _$ss7RawSpanV11byteOffsetsSnySiGvpMV
Added: _$ss7RawSpanV6_countSivg
Added: _$ss7RawSpanV6_countSivpMV
Added: _$ss7RawSpanV7isEmptySbvpMV
Added: _$ss7RawSpanV8_pointerSVSgvg
Added: _$ss7RawSpanV8_pointerSVSgvpMV
Added: _$ss7RawSpanV9byteCountSivpMV
Added: _$ss7RawSpanVMa
Added: _$ss7RawSpanVMn
Added: _$ss7RawSpanVN

// _SwiftifyInfo enum for _SwiftifyImports macro
Added: _$ss13_SwiftifyInfoO11nonescapingyABSi_tcABmFWC
Added: _$ss13_SwiftifyInfoO7endedByyABSi_SitcABmFWC
Added: _$ss13_SwiftifyInfoO7sizedByyABSi_SStcABmFWC
Added: _$ss13_SwiftifyInfoO9countedByyABSi_SStcABmFWC
Added: _$ss13_SwiftifyInfoOMa
Added: _$ss13_SwiftifyInfoOMn
Added: _$ss13_SwiftifyInfoON

//===----------------------------------------------------------------------===//
// Runtime Symbols
//===----------------------------------------------------------------------===//

// This shipped in Swift 6.0
Removed: _concurrencyEnableJobDispatchIntegration

Added: _swift_clearSensitive
Added: _swift_updatePureObjCClassMetadata
Added: _swift_initRawStructMetadata2
Added: _swift_getFixedArrayTypeMetadata
