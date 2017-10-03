//===--- main.swift -------------------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

////////////////////////////////////////////////////////////////////////////////
// WARNING: This file is manually generated from .gyb template and should not
// be directly modified. Instead, make changes to main.swift.gyb and run
// scripts/generate_harness/generate_harness.py to regenerate this file.
////////////////////////////////////////////////////////////////////////////////

// This is just a driver for performance overview tests.
import TestsUtils
import DriverUtils
import Ackermann
import AngryPhonebook
import AnyHashableWithAClass
import Array2D
import ArrayAppend
import ArrayInClass
import ArrayLiteral
import ArrayOfGenericPOD
import ArrayOfGenericRef
import ArrayOfPOD
import ArrayOfRef
import ArraySetElement
import ArraySubscript
import BitCount
import ByteSwap
import CString
import Calculator
import CaptureProp
import CharacterLiteralsLarge
import CharacterLiteralsSmall
import Chars
import ClassArrayGetter
import DeadArray
import DictTest
import DictTest2
import DictTest3
import DictionaryBridge
import DictionaryGroup
import DictionaryLiteral
import DictionaryRemove
import DictionarySwap
import DropFirst
import DropLast
import DropWhile
import ErrorHandling
import Exclusivity
import ExistentialPerformance
import Fibonacci
import Hanoi
import Hash
import HashQuadratic
import Histogram
import Integrate
import IterateData
import Join
import LazyFilter
import LinkedList
import MapReduce
import Memset
import MonteCarloE
import MonteCarloPi
import NSDictionaryCastToSwift
import NSError
import NSStringConversion
import NopDeinit
import ObjectAllocation
import ObjectiveCBridging
import ObjectiveCBridgingStubs
import ObjectiveCNoBridgingStubs
import ObserverClosure
import ObserverForwarderStruct
import ObserverPartiallyAppliedMethod
import ObserverUnappliedMethod
import OpenClose
import Phonebook
import PolymorphicCalls
import PopFront
import PopFrontGeneric
import Prefix
import PrefixWhile
import Prims
import PrimsSplit
import ProtocolDispatch
import ProtocolDispatch2
import RC4
import RGBHistogram
import RangeAssignment
import RecursiveOwnedParameter
import ReduceInto
import ReversedCollections
import SetTests
import SevenBoom
import Sim2DArray
import SortLargeExistentials
import SortLettersInPlace
import SortStrings
import StackPromo
import StaticArray
import StrComplexWalk
import StrToInt
import StringBuilder
import StringEdits
import StringEnum
import StringInterpolation
import StringMatch
import StringTests
import StringWalk
import Substring
import Suffix
import SuperChars
import TwoSum
import TypeFlood
import UTF8Decode
import Walsh
import XorLoop

@inline(__always)
private func registerBenchmark(_ bench: BenchmarkInfo) {
  registeredBenchmarks.append(bench)
}

@inline(__always)
private func registerBenchmark(
  _ name: String,
  _ function: @escaping (Int) -> (),
  _ tags: [BenchmarkCategory]
) {
  registerBenchmark(
    BenchmarkInfo(name: name, runFunction: function, tags: tags))
}

registerBenchmark(AnyHashableWithAClass)
registerBenchmark(ArraySetElement)
registerBenchmark(ExclusivityGlobal)
registerBenchmark(ExclusivityInMatSet)
registerBenchmark(ExclusivityIndependent)
registerBenchmark(LinkedList)
registerBenchmark(ObjectAllocation)
registerBenchmark(PolymorphicCalls)
registerBenchmark(SevenBoom)

// The main test suite: precommit tests
registerBenchmark("AngryPhonebook", run_AngryPhonebook, [.validation, .api, .String])
registerBenchmark("AnyHashableWithAClass", run_AnyHashableWithAClass, [.validation, .abstraction, .runtime])
registerBenchmark("Array2D", run_Array2D, [.validation, .api, .Array])
registerBenchmark("ArrayAppend", run_ArrayAppend, [.validation, .api, .Array])
registerBenchmark("ArrayAppendArrayOfInt", run_ArrayAppendArrayOfInt, [.validation, .api, .Array])
registerBenchmark("ArrayAppendAscii", run_ArrayAppendAscii, [.validation, .api, .Array])
registerBenchmark("ArrayAppendFromGeneric", run_ArrayAppendFromGeneric, [.validation, .api, .Array])
registerBenchmark("ArrayAppendGenericStructs", run_ArrayAppendGenericStructs, [.validation, .api, .Array])
registerBenchmark("ArrayAppendLatin1", run_ArrayAppendLatin1, [.validation, .api, .Array])
registerBenchmark("ArrayAppendLazyMap", run_ArrayAppendLazyMap, [.validation, .api, .Array])
registerBenchmark("ArrayAppendOptionals", run_ArrayAppendOptionals, [.validation, .api, .Array])
registerBenchmark("ArrayAppendRepeatCol", run_ArrayAppendRepeatCol, [.validation, .api, .Array])
registerBenchmark("ArrayAppendReserved", run_ArrayAppendReserved, [.validation, .api, .Array])
registerBenchmark("ArrayAppendSequence", run_ArrayAppendSequence, [.validation, .api, .Array])
registerBenchmark("ArrayAppendStrings", run_ArrayAppendStrings, [.validation, .api, .Array])
registerBenchmark("ArrayAppendToFromGeneric", run_ArrayAppendToFromGeneric, [.validation, .api, .Array])
registerBenchmark("ArrayAppendToGeneric", run_ArrayAppendToGeneric, [.validation, .api, .Array])
registerBenchmark("ArrayAppendUTF16", run_ArrayAppendUTF16, [.validation, .api, .Array])
registerBenchmark("ArrayInClass", run_ArrayInClass, [.validation, .api, .Array])
registerBenchmark("ArrayLiteral", run_ArrayLiteral, [.validation, .api, .Array])
registerBenchmark("ArrayOfGenericPOD", run_ArrayOfGenericPOD, [.validation, .api, .Array])
registerBenchmark("ArrayOfGenericRef", run_ArrayOfGenericRef, [.validation, .api, .Array])
registerBenchmark("ArrayOfPOD", run_ArrayOfPOD, [.validation, .api, .Array])
registerBenchmark("ArrayOfRef", run_ArrayOfRef, [.validation, .api, .Array])
registerBenchmark("ArrayPlusEqualArrayOfInt", run_ArrayPlusEqualArrayOfInt, [.validation, .api, .Array])
registerBenchmark("ArrayPlusEqualFiveElementCollection", run_ArrayPlusEqualFiveElementCollection, [.validation, .api, .Array])
registerBenchmark("ArrayPlusEqualSingleElementCollection", run_ArrayPlusEqualSingleElementCollection, [.validation, .api, .Array])
registerBenchmark("ArrayPlusEqualThreeElements", run_ArrayPlusEqualThreeElements, [.validation, .api, .Array])
registerBenchmark("ArraySubscript", run_ArraySubscript, [.validation, .api, .Array])
registerBenchmark("ArrayValueProp", run_ArrayValueProp, [.validation, .api, .Array])
registerBenchmark("ArrayValueProp2", run_ArrayValueProp2, [.validation, .api, .Array])
registerBenchmark("ArrayValueProp3", run_ArrayValueProp3, [.validation, .api, .Array])
registerBenchmark("ArrayValueProp4", run_ArrayValueProp4, [.validation, .api, .Array])
registerBenchmark("BitCount", run_BitCount, [.validation, .algorithm])
registerBenchmark("ByteSwap", run_ByteSwap, [.validation, .algorithm])
registerBenchmark("CStringLongAscii", run_CStringLongAscii, [.validation, .api, .String, .bridging])
registerBenchmark("CStringLongNonAscii", run_CStringLongNonAscii, [.validation, .api, .String, .bridging])
registerBenchmark("CStringShortAscii", run_CStringShortAscii, [.validation, .api, .String, .bridging])
registerBenchmark("Calculator", run_Calculator, [.validation])
registerBenchmark("CaptureProp", run_CaptureProp, [.validation, .api, .refcount])
registerBenchmark("CharIndexing_ascii_unicodeScalars", run_CharIndexing_ascii_unicodeScalars, [.validation, .api, .String])
registerBenchmark("CharIndexing_ascii_unicodeScalars_Backwards", run_CharIndexing_ascii_unicodeScalars_Backwards, [.validation, .api, .String])
registerBenchmark("CharIndexing_chinese_unicodeScalars", run_CharIndexing_chinese_unicodeScalars, [.validation, .api, .String])
registerBenchmark("CharIndexing_chinese_unicodeScalars_Backwards", run_CharIndexing_chinese_unicodeScalars_Backwards, [.validation, .api, .String])
registerBenchmark("CharIndexing_japanese_unicodeScalars", run_CharIndexing_japanese_unicodeScalars, [.validation, .api, .String])
registerBenchmark("CharIndexing_japanese_unicodeScalars_Backwards", run_CharIndexing_japanese_unicodeScalars_Backwards, [.validation, .api, .String])
registerBenchmark("CharIndexing_korean_unicodeScalars", run_CharIndexing_korean_unicodeScalars, [.validation, .api, .String])
registerBenchmark("CharIndexing_korean_unicodeScalars_Backwards", run_CharIndexing_korean_unicodeScalars_Backwards, [.validation, .api, .String])
registerBenchmark("CharIndexing_punctuatedJapanese_unicodeScalars", run_CharIndexing_punctuatedJapanese_unicodeScalars, [.validation, .api, .String])
registerBenchmark("CharIndexing_punctuatedJapanese_unicodeScalars_Backwards", run_CharIndexing_punctuatedJapanese_unicodeScalars_Backwards, [.validation, .api, .String])
registerBenchmark("CharIndexing_punctuated_unicodeScalars", run_CharIndexing_punctuated_unicodeScalars, [.validation, .api, .String])
registerBenchmark("CharIndexing_punctuated_unicodeScalars_Backwards", run_CharIndexing_punctuated_unicodeScalars_Backwards, [.validation, .api, .String])
registerBenchmark("CharIndexing_russian_unicodeScalars", run_CharIndexing_russian_unicodeScalars, [.validation, .api, .String])
registerBenchmark("CharIndexing_russian_unicodeScalars_Backwards", run_CharIndexing_russian_unicodeScalars_Backwards, [.validation, .api, .String])
registerBenchmark("CharIndexing_tweet_unicodeScalars", run_CharIndexing_tweet_unicodeScalars, [.validation, .api, .String])
registerBenchmark("CharIndexing_tweet_unicodeScalars_Backwards", run_CharIndexing_tweet_unicodeScalars_Backwards, [.validation, .api, .String])
registerBenchmark("CharIndexing_utf16_unicodeScalars", run_CharIndexing_utf16_unicodeScalars, [.validation, .api, .String])
registerBenchmark("CharIndexing_utf16_unicodeScalars_Backwards", run_CharIndexing_utf16_unicodeScalars_Backwards, [.validation, .api, .String])
registerBenchmark("CharIteration_ascii_unicodeScalars", run_CharIteration_ascii_unicodeScalars, [.validation, .api, .String])
registerBenchmark("CharIteration_ascii_unicodeScalars_Backwards", run_CharIteration_ascii_unicodeScalars_Backwards, [.validation, .api, .String])
registerBenchmark("CharIteration_chinese_unicodeScalars", run_CharIteration_chinese_unicodeScalars, [.validation, .api, .String])
registerBenchmark("CharIteration_chinese_unicodeScalars_Backwards", run_CharIteration_chinese_unicodeScalars_Backwards, [.validation, .api, .String])
registerBenchmark("CharIteration_japanese_unicodeScalars", run_CharIteration_japanese_unicodeScalars, [.validation, .api, .String])
registerBenchmark("CharIteration_japanese_unicodeScalars_Backwards", run_CharIteration_japanese_unicodeScalars_Backwards, [.validation, .api, .String])
registerBenchmark("CharIteration_korean_unicodeScalars", run_CharIteration_korean_unicodeScalars, [.validation, .api, .String])
registerBenchmark("CharIteration_korean_unicodeScalars_Backwards", run_CharIteration_korean_unicodeScalars_Backwards, [.validation, .api, .String])
registerBenchmark("CharIteration_punctuatedJapanese_unicodeScalars", run_CharIteration_punctuatedJapanese_unicodeScalars, [.validation, .api, .String])
registerBenchmark("CharIteration_punctuatedJapanese_unicodeScalars_Backwards", run_CharIteration_punctuatedJapanese_unicodeScalars_Backwards, [.validation, .api, .String])
registerBenchmark("CharIteration_punctuated_unicodeScalars", run_CharIteration_punctuated_unicodeScalars, [.validation, .api, .String])
registerBenchmark("CharIteration_punctuated_unicodeScalars_Backwards", run_CharIteration_punctuated_unicodeScalars_Backwards, [.validation, .api, .String])
registerBenchmark("CharIteration_russian_unicodeScalars", run_CharIteration_russian_unicodeScalars, [.validation, .api, .String])
registerBenchmark("CharIteration_russian_unicodeScalars_Backwards", run_CharIteration_russian_unicodeScalars_Backwards, [.validation, .api, .String])
registerBenchmark("CharIteration_tweet_unicodeScalars", run_CharIteration_tweet_unicodeScalars, [.validation, .api, .String])
registerBenchmark("CharIteration_tweet_unicodeScalars_Backwards", run_CharIteration_tweet_unicodeScalars_Backwards, [.validation, .api, .String])
registerBenchmark("CharIteration_utf16_unicodeScalars", run_CharIteration_utf16_unicodeScalars, [.validation, .api, .String])
registerBenchmark("CharIteration_utf16_unicodeScalars_Backwards", run_CharIteration_utf16_unicodeScalars_Backwards, [.validation, .api, .String])
registerBenchmark("CharacterLiteralsLarge", run_CharacterLiteralsLarge, [.validation, .api, .String])
registerBenchmark("CharacterLiteralsSmall", run_CharacterLiteralsSmall, [.validation, .api, .String])
registerBenchmark("Chars", run_Chars, [.validation, .api, .String])
registerBenchmark("ClassArrayGetter", run_ClassArrayGetter, [.validation, .api, .Array])
registerBenchmark("DeadArray", run_DeadArray, [.regression])
registerBenchmark("Dictionary", run_Dictionary, [.validation, .api, .Dictionary])
registerBenchmark("Dictionary2", run_Dictionary2, [.validation, .api, .Dictionary])
registerBenchmark("Dictionary2OfObjects", run_Dictionary2OfObjects, [.validation, .api, .Dictionary])
registerBenchmark("Dictionary3", run_Dictionary3, [.validation, .api, .Dictionary])
registerBenchmark("Dictionary3OfObjects", run_Dictionary3OfObjects, [.validation, .api, .Dictionary])
registerBenchmark("DictionaryBridge", run_DictionaryBridge, [.validation, .api, .Dictionary, .bridging])
registerBenchmark("DictionaryGroup", run_DictionaryGroup, [.validation, .api, .Dictionary])
registerBenchmark("DictionaryGroupOfObjects", run_DictionaryGroupOfObjects, [.validation, .api, .Dictionary])
registerBenchmark("DictionaryLiteral", run_DictionaryLiteral, [.validation, .api, .Dictionary])
registerBenchmark("DictionaryOfObjects", run_DictionaryOfObjects, [.validation, .api, .Dictionary])
registerBenchmark("DictionaryRemove", run_DictionaryRemove, [.validation, .api, .Dictionary])
registerBenchmark("DictionaryRemoveOfObjects", run_DictionaryRemoveOfObjects, [.validation, .api, .Dictionary])
registerBenchmark("DictionarySwap", run_DictionarySwap, [.validation, .api, .Dictionary])
registerBenchmark("DictionarySwapOfObjects", run_DictionarySwapOfObjects, [.validation, .api, .Dictionary])
registerBenchmark("DropFirstAnyCollection", run_DropFirstAnyCollection, [.validation, .api])
registerBenchmark("DropFirstAnyCollectionLazy", run_DropFirstAnyCollectionLazy, [.validation, .api])
registerBenchmark("DropFirstAnySeqCRangeIter", run_DropFirstAnySeqCRangeIter, [.validation, .api])
registerBenchmark("DropFirstAnySeqCRangeIterLazy", run_DropFirstAnySeqCRangeIterLazy, [.validation, .api])
registerBenchmark("DropFirstAnySeqCntRange", run_DropFirstAnySeqCntRange, [.validation, .api])
registerBenchmark("DropFirstAnySeqCntRangeLazy", run_DropFirstAnySeqCntRangeLazy, [.validation, .api])
registerBenchmark("DropFirstAnySequence", run_DropFirstAnySequence, [.validation, .api])
registerBenchmark("DropFirstAnySequenceLazy", run_DropFirstAnySequenceLazy, [.validation, .api])
registerBenchmark("DropFirstArray", run_DropFirstArray, [.validation, .api, .Array])
registerBenchmark("DropFirstArrayLazy", run_DropFirstArrayLazy, [.validation, .api, .Array])
registerBenchmark("DropFirstCountableRange", run_DropFirstCountableRange, [.validation, .api])
registerBenchmark("DropFirstCountableRangeLazy", run_DropFirstCountableRangeLazy, [.validation, .api])
registerBenchmark("DropFirstSequence", run_DropFirstSequence, [.validation, .api])
registerBenchmark("DropFirstSequenceLazy", run_DropFirstSequenceLazy, [.validation, .api])
registerBenchmark("DropLastAnyCollection", run_DropLastAnyCollection, [.validation, .api])
registerBenchmark("DropLastAnyCollectionLazy", run_DropLastAnyCollectionLazy, [.validation, .api])
registerBenchmark("DropLastAnySeqCRangeIter", run_DropLastAnySeqCRangeIter, [.validation, .api])
registerBenchmark("DropLastAnySeqCRangeIterLazy", run_DropLastAnySeqCRangeIterLazy, [.validation, .api])
registerBenchmark("DropLastAnySeqCntRange", run_DropLastAnySeqCntRange, [.validation, .api])
registerBenchmark("DropLastAnySeqCntRangeLazy", run_DropLastAnySeqCntRangeLazy, [.validation, .api])
registerBenchmark("DropLastAnySequence", run_DropLastAnySequence, [.validation, .api])
registerBenchmark("DropLastAnySequenceLazy", run_DropLastAnySequenceLazy, [.validation, .api])
registerBenchmark("DropLastArray", run_DropLastArray, [.validation, .api, .Array])
registerBenchmark("DropLastArrayLazy", run_DropLastArrayLazy, [.validation, .api, .Array])
registerBenchmark("DropLastCountableRange", run_DropLastCountableRange, [.validation, .api])
registerBenchmark("DropLastCountableRangeLazy", run_DropLastCountableRangeLazy, [.validation, .api])
registerBenchmark("DropLastSequence", run_DropLastSequence, [.validation, .api])
registerBenchmark("DropLastSequenceLazy", run_DropLastSequenceLazy, [.validation, .api])
registerBenchmark("DropWhileAnyCollection", run_DropWhileAnyCollection, [.validation, .api])
registerBenchmark("DropWhileAnyCollectionLazy", run_DropWhileAnyCollectionLazy, [.validation, .api])
registerBenchmark("DropWhileAnySeqCRangeIter", run_DropWhileAnySeqCRangeIter, [.validation, .api])
registerBenchmark("DropWhileAnySeqCRangeIterLazy", run_DropWhileAnySeqCRangeIterLazy, [.validation, .api])
registerBenchmark("DropWhileAnySeqCntRange", run_DropWhileAnySeqCntRange, [.validation, .api])
registerBenchmark("DropWhileAnySeqCntRangeLazy", run_DropWhileAnySeqCntRangeLazy, [.validation, .api])
registerBenchmark("DropWhileAnySequence", run_DropWhileAnySequence, [.validation, .api])
registerBenchmark("DropWhileAnySequenceLazy", run_DropWhileAnySequenceLazy, [.validation, .api])
registerBenchmark("DropWhileArray", run_DropWhileArray, [.validation, .api, .Array])
registerBenchmark("DropWhileArrayLazy", run_DropWhileArrayLazy, [.validation, .api, .Array])
registerBenchmark("DropWhileCountableRange", run_DropWhileCountableRange, [.validation, .api])
registerBenchmark("DropWhileCountableRangeLazy", run_DropWhileCountableRangeLazy, [.validation, .api])
registerBenchmark("DropWhileSequence", run_DropWhileSequence, [.validation, .api])
registerBenchmark("DropWhileSequenceLazy", run_DropWhileSequenceLazy, [.validation, .api])
registerBenchmark("EqualStringSubstring", run_EqualStringSubstring, [.validation, .api, .String])
registerBenchmark("EqualSubstringString", run_EqualSubstringString, [.validation, .api, .String])
registerBenchmark("EqualSubstringSubstring", run_EqualSubstringSubstring, [.validation, .api, .String])
registerBenchmark("EqualSubstringSubstringGenericEquatable", run_EqualSubstringSubstringGenericEquatable, [.validation, .api, .String])
registerBenchmark("ErrorHandling", run_ErrorHandling, [.validation, .exceptions])
registerBenchmark("FilterEvenUsingReduce", run_FilterEvenUsingReduce, [.validation, .api])
registerBenchmark("FilterEvenUsingReduceInto", run_FilterEvenUsingReduceInto, [.validation, .api])
registerBenchmark("FrequenciesUsingReduce", run_FrequenciesUsingReduce, [.validation, .api])
registerBenchmark("FrequenciesUsingReduceInto", run_FrequenciesUsingReduceInto, [.validation, .api])
registerBenchmark("Hanoi", run_Hanoi, [.validation, .algorithm])
registerBenchmark("HashTest", run_HashTest, [.validation, .algorithm])
registerBenchmark("Histogram", run_Histogram, [.validation, .algorithm])
registerBenchmark("Integrate", run_Integrate, [.validation, .algorithm])
registerBenchmark("IterateData", run_IterateData, [.validation, .api])
registerBenchmark("Join", run_Join, [.validation, .api, .String, .Array])
registerBenchmark("LazilyFilteredArrays", run_LazilyFilteredArrays, [.validation, .api, .Array])
registerBenchmark("LazilyFilteredRange", run_LazilyFilteredRange, [.validation, .api, .Array])
registerBenchmark("LessSubstringSubstring", run_LessSubstringSubstring, [.validation, .api, .String])
registerBenchmark("LessSubstringSubstringGenericComparable", run_LessSubstringSubstringGenericComparable, [.validation, .api, .String])
registerBenchmark("LinkedList", run_LinkedList, [.validation])
registerBenchmark("MapReduce", run_MapReduce, [.validation, .algorithm])
registerBenchmark("MapReduceAnyCollection", run_MapReduceAnyCollection, [.validation, .algorithm])
registerBenchmark("MapReduceAnyCollectionShort", run_MapReduceAnyCollectionShort, [.validation, .algorithm])
registerBenchmark("MapReduceClass", run_MapReduceClass, [.validation, .algorithm])
registerBenchmark("MapReduceClassShort", run_MapReduceClassShort, [.validation, .algorithm])
registerBenchmark("MapReduceLazyCollection", run_MapReduceLazyCollection, [.validation, .algorithm])
registerBenchmark("MapReduceLazyCollectionShort", run_MapReduceLazyCollectionShort, [.validation, .algorithm])
registerBenchmark("MapReduceLazySequence", run_MapReduceLazySequence, [.validation, .algorithm])
registerBenchmark("MapReduceSequence", run_MapReduceSequence, [.validation, .algorithm])
registerBenchmark("MapReduceShort", run_MapReduceShort, [.validation, .algorithm])
registerBenchmark("MapReduceShortString", run_MapReduceShortString, [.validation, .algorithm, .String])
registerBenchmark("MapReduceString", run_MapReduceString, [.validation, .algorithm, .String])
registerBenchmark("Memset", run_Memset, [.validation])
registerBenchmark("MonteCarloE", run_MonteCarloE, [.validation, .algorithm])
registerBenchmark("MonteCarloPi", run_MonteCarloPi, [.validation, .algorithm])
registerBenchmark("NSDictionaryCastToSwift", run_NSDictionaryCastToSwift, [.validation, .api, .Dictionary, .bridging])
registerBenchmark("NSError", run_NSError, [.validation, .exceptions, .bridging])
registerBenchmark("NSStringConversion", run_NSStringConversion, [.validation, .api, .String, .bridging])
registerBenchmark("NopDeinit", run_NopDeinit, [.regression])
registerBenchmark("ObjectAllocation", run_ObjectAllocation, [.validation])
registerBenchmark("ObjectiveCBridgeFromNSArrayAnyObject", run_ObjectiveCBridgeFromNSArrayAnyObject, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeFromNSArrayAnyObjectForced", run_ObjectiveCBridgeFromNSArrayAnyObjectForced, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeFromNSArrayAnyObjectToString", run_ObjectiveCBridgeFromNSArrayAnyObjectToString, [.validation, .bridging, .String])
registerBenchmark("ObjectiveCBridgeFromNSArrayAnyObjectToStringForced", run_ObjectiveCBridgeFromNSArrayAnyObjectToStringForced, [.validation, .bridging, .String])
registerBenchmark("ObjectiveCBridgeFromNSDictionaryAnyObject", run_ObjectiveCBridgeFromNSDictionaryAnyObject, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeFromNSDictionaryAnyObjectForced", run_ObjectiveCBridgeFromNSDictionaryAnyObjectForced, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeFromNSDictionaryAnyObjectToString", run_ObjectiveCBridgeFromNSDictionaryAnyObjectToString, [.validation, .bridging, .String])
registerBenchmark("ObjectiveCBridgeFromNSDictionaryAnyObjectToStringForced", run_ObjectiveCBridgeFromNSDictionaryAnyObjectToStringForced, [.validation, .bridging, .String])
registerBenchmark("ObjectiveCBridgeFromNSSetAnyObject", run_ObjectiveCBridgeFromNSSetAnyObject, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeFromNSSetAnyObjectForced", run_ObjectiveCBridgeFromNSSetAnyObjectForced, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeFromNSSetAnyObjectToString", run_ObjectiveCBridgeFromNSSetAnyObjectToString, [.validation, .bridging, .String])
registerBenchmark("ObjectiveCBridgeFromNSSetAnyObjectToStringForced", run_ObjectiveCBridgeFromNSSetAnyObjectToStringForced, [.validation, .bridging, .String])
registerBenchmark("ObjectiveCBridgeFromNSString", run_ObjectiveCBridgeFromNSString, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeFromNSStringForced", run_ObjectiveCBridgeFromNSStringForced, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeStubDataAppend", run_ObjectiveCBridgeStubDataAppend, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeStubDateAccess", run_ObjectiveCBridgeStubDateAccess, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeStubDateMutation", run_ObjectiveCBridgeStubDateMutation, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeStubFromArrayOfNSString", run_ObjectiveCBridgeStubFromArrayOfNSString, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeStubFromNSDate", run_ObjectiveCBridgeStubFromNSDate, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeStubFromNSDateRef", run_ObjectiveCBridgeStubFromNSDateRef, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeStubFromNSString", run_ObjectiveCBridgeStubFromNSString, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeStubFromNSStringRef", run_ObjectiveCBridgeStubFromNSStringRef, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeStubNSDataAppend", run_ObjectiveCBridgeStubNSDataAppend, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeStubNSDateMutationRef", run_ObjectiveCBridgeStubNSDateMutationRef, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeStubNSDateRefAccess", run_ObjectiveCBridgeStubNSDateRefAccess, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeStubToArrayOfNSString", run_ObjectiveCBridgeStubToArrayOfNSString, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeStubToNSDate", run_ObjectiveCBridgeStubToNSDate, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeStubToNSDateRef", run_ObjectiveCBridgeStubToNSDateRef, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeStubToNSString", run_ObjectiveCBridgeStubToNSString, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeStubToNSStringRef", run_ObjectiveCBridgeStubToNSStringRef, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeStubURLAppendPath", run_ObjectiveCBridgeStubURLAppendPath, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeStubURLAppendPathRef", run_ObjectiveCBridgeStubURLAppendPathRef, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeToNSArray", run_ObjectiveCBridgeToNSArray, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeToNSDictionary", run_ObjectiveCBridgeToNSDictionary, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeToNSSet", run_ObjectiveCBridgeToNSSet, [.validation, .bridging])
registerBenchmark("ObjectiveCBridgeToNSString", run_ObjectiveCBridgeToNSString, [.validation, .bridging])
registerBenchmark("ObserverClosure", run_ObserverClosure, [.validation])
registerBenchmark("ObserverForwarderStruct", run_ObserverForwarderStruct, [.validation])
registerBenchmark("ObserverPartiallyAppliedMethod", run_ObserverPartiallyAppliedMethod, [.validation])
registerBenchmark("ObserverUnappliedMethod", run_ObserverUnappliedMethod, [.validation])
registerBenchmark("OpenClose", run_OpenClose, [.validation, .api, .String])
registerBenchmark("Phonebook", run_Phonebook, [.validation, .api, .String])
registerBenchmark("PolymorphicCalls", run_PolymorphicCalls, [.validation, .runtime, .abstraction])
registerBenchmark("PopFrontArray", run_PopFrontArray, [.validation, .api, .Array])
registerBenchmark("PopFrontArrayGeneric", run_PopFrontArrayGeneric, [.validation, .api, .Array])
registerBenchmark("PopFrontUnsafePointer", run_PopFrontUnsafePointer, [.validation, .api])
registerBenchmark("PrefixAnyCollection", run_PrefixAnyCollection, [.validation, .api])
registerBenchmark("PrefixAnyCollectionLazy", run_PrefixAnyCollectionLazy, [.validation, .api])
registerBenchmark("PrefixAnySeqCRangeIter", run_PrefixAnySeqCRangeIter, [.validation, .api])
registerBenchmark("PrefixAnySeqCRangeIterLazy", run_PrefixAnySeqCRangeIterLazy, [.validation, .api])
registerBenchmark("PrefixAnySeqCntRange", run_PrefixAnySeqCntRange, [.validation, .api])
registerBenchmark("PrefixAnySeqCntRangeLazy", run_PrefixAnySeqCntRangeLazy, [.validation, .api])
registerBenchmark("PrefixAnySequence", run_PrefixAnySequence, [.validation, .api])
registerBenchmark("PrefixAnySequenceLazy", run_PrefixAnySequenceLazy, [.validation, .api])
registerBenchmark("PrefixArray", run_PrefixArray, [.validation, .api, .Array])
registerBenchmark("PrefixArrayLazy", run_PrefixArrayLazy, [.validation, .api, .Array])
registerBenchmark("PrefixCountableRange", run_PrefixCountableRange, [.validation, .api])
registerBenchmark("PrefixCountableRangeLazy", run_PrefixCountableRangeLazy, [.validation, .api])
registerBenchmark("PrefixSequence", run_PrefixSequence, [.validation, .api])
registerBenchmark("PrefixSequenceLazy", run_PrefixSequenceLazy, [.validation, .api])
registerBenchmark("PrefixWhileAnyCollection", run_PrefixWhileAnyCollection, [.validation, .api])
registerBenchmark("PrefixWhileAnyCollectionLazy", run_PrefixWhileAnyCollectionLazy, [.validation, .api])
registerBenchmark("PrefixWhileAnySeqCRangeIter", run_PrefixWhileAnySeqCRangeIter, [.validation, .api])
registerBenchmark("PrefixWhileAnySeqCRangeIterLazy", run_PrefixWhileAnySeqCRangeIterLazy, [.validation, .api])
registerBenchmark("PrefixWhileAnySeqCntRange", run_PrefixWhileAnySeqCntRange, [.validation, .api])
registerBenchmark("PrefixWhileAnySeqCntRangeLazy", run_PrefixWhileAnySeqCntRangeLazy, [.validation, .api])
registerBenchmark("PrefixWhileAnySequence", run_PrefixWhileAnySequence, [.validation, .api])
registerBenchmark("PrefixWhileAnySequenceLazy", run_PrefixWhileAnySequenceLazy, [.validation, .api])
registerBenchmark("PrefixWhileArray", run_PrefixWhileArray, [.validation, .api, .Array])
registerBenchmark("PrefixWhileArrayLazy", run_PrefixWhileArrayLazy, [.validation, .api, .Array])
registerBenchmark("PrefixWhileCountableRange", run_PrefixWhileCountableRange, [.validation, .api])
registerBenchmark("PrefixWhileCountableRangeLazy", run_PrefixWhileCountableRangeLazy, [.validation, .api])
registerBenchmark("PrefixWhileSequence", run_PrefixWhileSequence, [.validation, .api])
registerBenchmark("PrefixWhileSequenceLazy", run_PrefixWhileSequenceLazy, [.validation, .api])
registerBenchmark("Prims", run_Prims, [.validation, .algorithm])
registerBenchmark("PrimsSplit", run_PrimsSplit, [.validation, .algorithm])
registerBenchmark("ProtocolDispatch", run_ProtocolDispatch, [.validation, .abstraction])
registerBenchmark("ProtocolDispatch2", run_ProtocolDispatch2, [.validation, .abstraction])
registerBenchmark("RC4", run_RC4, [.validation, .algorithm])
registerBenchmark("RGBHistogram", run_RGBHistogram, [.validation, .algorithm])
registerBenchmark("RGBHistogramOfObjects", run_RGBHistogramOfObjects, [.validation, .algorithm])
registerBenchmark("RangeAssignment", run_RangeAssignment, [.validation, .api])
registerBenchmark("RecursiveOwnedParameter", run_RecursiveOwnedParameter, [.validation, .api, .Array, .refcount])
registerBenchmark("ReversedArray", run_ReversedArray, [.validation, .api, .Array])
registerBenchmark("ReversedBidirectional", run_ReversedBidirectional, [.validation, .api])
registerBenchmark("ReversedDictionary", run_ReversedDictionary, [.validation, .api, .Dictionary])
registerBenchmark("SetExclusiveOr", run_SetExclusiveOr, [.validation, .api, .Set])
registerBenchmark("SetExclusiveOr_OfObjects", run_SetExclusiveOr_OfObjects, [.validation, .api, .Set])
registerBenchmark("SetIntersect", run_SetIntersect, [.validation, .api, .Set])
registerBenchmark("SetIntersect_OfObjects", run_SetIntersect_OfObjects, [.validation, .api, .Set])
registerBenchmark("SetIsSubsetOf", run_SetIsSubsetOf, [.validation, .api, .Set])
registerBenchmark("SetIsSubsetOf_OfObjects", run_SetIsSubsetOf_OfObjects, [.validation, .api, .Set])
registerBenchmark("SetUnion", run_SetUnion, [.validation, .api, .Set])
registerBenchmark("SetUnion_OfObjects", run_SetUnion_OfObjects, [.validation, .api, .Set])
registerBenchmark("SevenBoom", run_SevenBoom, [.validation])
registerBenchmark("Sim2DArray", run_Sim2DArray, [.validation, .api, .Array])
registerBenchmark("SortLargeExistentials", run_SortLargeExistentials, [.validation, .api, .algorithm])
registerBenchmark("SortLettersInPlace", run_SortLettersInPlace, [.validation, .api, .algorithm, .String])
registerBenchmark("SortSortedStrings", run_SortSortedStrings, [.validation, .api, .algorithm, .String])
registerBenchmark("SortStrings", run_SortStrings, [.validation, .api, .algorithm, .String])
registerBenchmark("SortStringsUnicode", run_SortStringsUnicode, [.validation, .api, .algorithm, .String])
registerBenchmark("StackPromo", run_StackPromo, [.regression])
registerBenchmark("StaticArray", run_StaticArray, [.validation, .api, .Array])
registerBenchmark("StrComplexWalk", run_StrComplexWalk, [.validation, .api, .String])
registerBenchmark("StrToInt", run_StrToInt, [.validation, .api, .String])
registerBenchmark("StringAdder", run_StringAdder, [.validation, .api, .String])
registerBenchmark("StringBuilder", run_StringBuilder, [.validation, .api, .String])
registerBenchmark("StringBuilderLong", run_StringBuilderLong, [.validation, .api, .String])
registerBenchmark("StringEdits", run_StringEdits, [.validation, .api, .String])
registerBenchmark("StringEnumRawValueInitialization", run_StringEnumRawValueInitialization, [.validation, .api, .String])
registerBenchmark("StringEqualPointerComparison", run_StringEqualPointerComparison, [.validation, .api, .String])
registerBenchmark("StringFromLongWholeSubstring", run_StringFromLongWholeSubstring, [.validation, .api, .String])
registerBenchmark("StringFromLongWholeSubstringGeneric", run_StringFromLongWholeSubstringGeneric, [.validation, .api, .String])
registerBenchmark("StringHasPrefix", run_StringHasPrefix, [.validation, .api, .String])
registerBenchmark("StringHasPrefixUnicode", run_StringHasPrefixUnicode, [.validation, .api, .String])
registerBenchmark("StringHasSuffix", run_StringHasSuffix, [.validation, .api, .String])
registerBenchmark("StringHasSuffixUnicode", run_StringHasSuffixUnicode, [.validation, .api, .String])
registerBenchmark("StringInterpolation", run_StringInterpolation, [.validation, .api, .String])
registerBenchmark("StringMatch", run_StringMatch, [.validation, .api, .String])
registerBenchmark("StringUTF16Builder", run_StringUTF16Builder, [.validation, .api, .String])
registerBenchmark("StringWalk", run_StringWalk, [.validation, .api, .String])
registerBenchmark("StringWithCString", run_StringWithCString, [.validation, .api, .String, .bridging])
registerBenchmark("SubstringComparable", run_SubstringComparable, [.validation, .api, .String])
registerBenchmark("SubstringEqualString", run_SubstringEqualString, [.validation, .api, .String])
registerBenchmark("SubstringEquatable", run_SubstringEquatable, [.validation, .api, .String])
registerBenchmark("SubstringFromLongString", run_SubstringFromLongString, [.validation, .api, .String])
registerBenchmark("SubstringFromLongStringGeneric", run_SubstringFromLongStringGeneric, [.validation, .api, .String])
registerBenchmark("SuffixAnyCollection", run_SuffixAnyCollection, [.validation, .api])
registerBenchmark("SuffixAnyCollectionLazy", run_SuffixAnyCollectionLazy, [.validation, .api])
registerBenchmark("SuffixAnySeqCRangeIter", run_SuffixAnySeqCRangeIter, [.validation, .api])
registerBenchmark("SuffixAnySeqCRangeIterLazy", run_SuffixAnySeqCRangeIterLazy, [.validation, .api])
registerBenchmark("SuffixAnySeqCntRange", run_SuffixAnySeqCntRange, [.validation, .api])
registerBenchmark("SuffixAnySeqCntRangeLazy", run_SuffixAnySeqCntRangeLazy, [.validation, .api])
registerBenchmark("SuffixAnySequence", run_SuffixAnySequence, [.validation, .api])
registerBenchmark("SuffixAnySequenceLazy", run_SuffixAnySequenceLazy, [.validation, .api])
registerBenchmark("SuffixArray", run_SuffixArray, [.validation, .api, .Array])
registerBenchmark("SuffixArrayLazy", run_SuffixArrayLazy, [.validation, .api, .Array])
registerBenchmark("SuffixCountableRange", run_SuffixCountableRange, [.validation, .api])
registerBenchmark("SuffixCountableRangeLazy", run_SuffixCountableRangeLazy, [.validation, .api])
registerBenchmark("SuffixSequence", run_SuffixSequence, [.validation, .api])
registerBenchmark("SuffixSequenceLazy", run_SuffixSequenceLazy, [.validation, .api])
registerBenchmark("SumUsingReduce", run_SumUsingReduce, [.validation, .api])
registerBenchmark("SumUsingReduceInto", run_SumUsingReduceInto, [.validation, .api])
registerBenchmark("SuperChars", run_SuperChars, [.validation, .api, .String])
registerBenchmark("TwoSum", run_TwoSum, [.validation, .api, .Dictionary, .Array, .algorithm])
registerBenchmark("TypeFlood", run_TypeFlood, [.validation, .metadata])
registerBenchmark("UTF8Decode", run_UTF8Decode, [.validation, .api, .String])
registerBenchmark("Walsh", run_Walsh, [.validation, .algorithm])
registerBenchmark("XorLoop", run_XorLoop, [.validation])
registerBenchmark("accessGlobal", run_accessGlobal, [.regression])
registerBenchmark("accessInMatSet", run_accessInMatSet, [.regression])
registerBenchmark("accessIndependent", run_accessIndependent, [.regression])

// Other tests
registerBenchmark("Ackermann", run_Ackermann, [.unstable, .algorithm])
registerBenchmark("ExistentialTestArrayConditionalShift_ClassValueBuffer1", run_ExistentialTestArrayConditionalShift_ClassValueBuffer1, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayConditionalShift_ClassValueBuffer2", run_ExistentialTestArrayConditionalShift_ClassValueBuffer2, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayConditionalShift_ClassValueBuffer3", run_ExistentialTestArrayConditionalShift_ClassValueBuffer3, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayConditionalShift_ClassValueBuffer4", run_ExistentialTestArrayConditionalShift_ClassValueBuffer4, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayConditionalShift_IntValueBuffer0", run_ExistentialTestArrayConditionalShift_IntValueBuffer0, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayConditionalShift_IntValueBuffer1", run_ExistentialTestArrayConditionalShift_IntValueBuffer1, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayConditionalShift_IntValueBuffer2", run_ExistentialTestArrayConditionalShift_IntValueBuffer2, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayConditionalShift_IntValueBuffer3", run_ExistentialTestArrayConditionalShift_IntValueBuffer3, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayConditionalShift_IntValueBuffer4", run_ExistentialTestArrayConditionalShift_IntValueBuffer4, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayMutating_ClassValueBuffer1", run_ExistentialTestArrayMutating_ClassValueBuffer1, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayMutating_ClassValueBuffer2", run_ExistentialTestArrayMutating_ClassValueBuffer2, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayMutating_ClassValueBuffer3", run_ExistentialTestArrayMutating_ClassValueBuffer3, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayMutating_ClassValueBuffer4", run_ExistentialTestArrayMutating_ClassValueBuffer4, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayMutating_IntValueBuffer0", run_ExistentialTestArrayMutating_IntValueBuffer0, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayMutating_IntValueBuffer1", run_ExistentialTestArrayMutating_IntValueBuffer1, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayMutating_IntValueBuffer2", run_ExistentialTestArrayMutating_IntValueBuffer2, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayMutating_IntValueBuffer3", run_ExistentialTestArrayMutating_IntValueBuffer3, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayMutating_IntValueBuffer4", run_ExistentialTestArrayMutating_IntValueBuffer4, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayOneMethodCall_ClassValueBuffer1", run_ExistentialTestArrayOneMethodCall_ClassValueBuffer1, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayOneMethodCall_ClassValueBuffer2", run_ExistentialTestArrayOneMethodCall_ClassValueBuffer2, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayOneMethodCall_ClassValueBuffer3", run_ExistentialTestArrayOneMethodCall_ClassValueBuffer3, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayOneMethodCall_ClassValueBuffer4", run_ExistentialTestArrayOneMethodCall_ClassValueBuffer4, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayOneMethodCall_IntValueBuffer0", run_ExistentialTestArrayOneMethodCall_IntValueBuffer0, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayOneMethodCall_IntValueBuffer1", run_ExistentialTestArrayOneMethodCall_IntValueBuffer1, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayOneMethodCall_IntValueBuffer2", run_ExistentialTestArrayOneMethodCall_IntValueBuffer2, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayOneMethodCall_IntValueBuffer3", run_ExistentialTestArrayOneMethodCall_IntValueBuffer3, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayOneMethodCall_IntValueBuffer4", run_ExistentialTestArrayOneMethodCall_IntValueBuffer4, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayShift_ClassValueBuffer1", run_ExistentialTestArrayShift_ClassValueBuffer1, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayShift_ClassValueBuffer2", run_ExistentialTestArrayShift_ClassValueBuffer2, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayShift_ClassValueBuffer3", run_ExistentialTestArrayShift_ClassValueBuffer3, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayShift_ClassValueBuffer4", run_ExistentialTestArrayShift_ClassValueBuffer4, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayShift_IntValueBuffer0", run_ExistentialTestArrayShift_IntValueBuffer0, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayShift_IntValueBuffer1", run_ExistentialTestArrayShift_IntValueBuffer1, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayShift_IntValueBuffer2", run_ExistentialTestArrayShift_IntValueBuffer2, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayShift_IntValueBuffer3", run_ExistentialTestArrayShift_IntValueBuffer3, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayShift_IntValueBuffer4", run_ExistentialTestArrayShift_IntValueBuffer4, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayTwoMethodCalls_ClassValueBuffer1", run_ExistentialTestArrayTwoMethodCalls_ClassValueBuffer1, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayTwoMethodCalls_ClassValueBuffer2", run_ExistentialTestArrayTwoMethodCalls_ClassValueBuffer2, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayTwoMethodCalls_ClassValueBuffer3", run_ExistentialTestArrayTwoMethodCalls_ClassValueBuffer3, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayTwoMethodCalls_ClassValueBuffer4", run_ExistentialTestArrayTwoMethodCalls_ClassValueBuffer4, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayTwoMethodCalls_IntValueBuffer0", run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer0, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayTwoMethodCalls_IntValueBuffer1", run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer1, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayTwoMethodCalls_IntValueBuffer2", run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer2, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayTwoMethodCalls_IntValueBuffer3", run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer3, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestArrayTwoMethodCalls_IntValueBuffer4", run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer4, [.unstable, .api, .Array])
registerBenchmark("ExistentialTestMutatingAndNonMutating_ClassValueBuffer1", run_ExistentialTestMutatingAndNonMutating_ClassValueBuffer1, [.unstable])
registerBenchmark("ExistentialTestMutatingAndNonMutating_ClassValueBuffer2", run_ExistentialTestMutatingAndNonMutating_ClassValueBuffer2, [.unstable])
registerBenchmark("ExistentialTestMutatingAndNonMutating_ClassValueBuffer3", run_ExistentialTestMutatingAndNonMutating_ClassValueBuffer3, [.unstable])
registerBenchmark("ExistentialTestMutatingAndNonMutating_ClassValueBuffer4", run_ExistentialTestMutatingAndNonMutating_ClassValueBuffer4, [.unstable])
registerBenchmark("ExistentialTestMutatingAndNonMutating_IntValueBuffer0", run_ExistentialTestMutatingAndNonMutating_IntValueBuffer0, [.unstable])
registerBenchmark("ExistentialTestMutatingAndNonMutating_IntValueBuffer1", run_ExistentialTestMutatingAndNonMutating_IntValueBuffer1, [.unstable])
registerBenchmark("ExistentialTestMutatingAndNonMutating_IntValueBuffer2", run_ExistentialTestMutatingAndNonMutating_IntValueBuffer2, [.unstable])
registerBenchmark("ExistentialTestMutatingAndNonMutating_IntValueBuffer3", run_ExistentialTestMutatingAndNonMutating_IntValueBuffer3, [.unstable])
registerBenchmark("ExistentialTestMutatingAndNonMutating_IntValueBuffer4", run_ExistentialTestMutatingAndNonMutating_IntValueBuffer4, [.unstable])
registerBenchmark("ExistentialTestMutating_ClassValueBuffer1", run_ExistentialTestMutating_ClassValueBuffer1, [.unstable])
registerBenchmark("ExistentialTestMutating_ClassValueBuffer2", run_ExistentialTestMutating_ClassValueBuffer2, [.unstable])
registerBenchmark("ExistentialTestMutating_ClassValueBuffer3", run_ExistentialTestMutating_ClassValueBuffer3, [.unstable])
registerBenchmark("ExistentialTestMutating_ClassValueBuffer4", run_ExistentialTestMutating_ClassValueBuffer4, [.unstable])
registerBenchmark("ExistentialTestMutating_IntValueBuffer0", run_ExistentialTestMutating_IntValueBuffer0, [.unstable])
registerBenchmark("ExistentialTestMutating_IntValueBuffer1", run_ExistentialTestMutating_IntValueBuffer1, [.unstable])
registerBenchmark("ExistentialTestMutating_IntValueBuffer2", run_ExistentialTestMutating_IntValueBuffer2, [.unstable])
registerBenchmark("ExistentialTestMutating_IntValueBuffer3", run_ExistentialTestMutating_IntValueBuffer3, [.unstable])
registerBenchmark("ExistentialTestMutating_IntValueBuffer4", run_ExistentialTestMutating_IntValueBuffer4, [.unstable])
registerBenchmark("ExistentialTestOneMethodCall_ClassValueBuffer1", run_ExistentialTestOneMethodCall_ClassValueBuffer1, [.unstable])
registerBenchmark("ExistentialTestOneMethodCall_ClassValueBuffer2", run_ExistentialTestOneMethodCall_ClassValueBuffer2, [.unstable])
registerBenchmark("ExistentialTestOneMethodCall_ClassValueBuffer3", run_ExistentialTestOneMethodCall_ClassValueBuffer3, [.unstable])
registerBenchmark("ExistentialTestOneMethodCall_ClassValueBuffer4", run_ExistentialTestOneMethodCall_ClassValueBuffer4, [.unstable])
registerBenchmark("ExistentialTestOneMethodCall_IntValueBuffer0", run_ExistentialTestOneMethodCall_IntValueBuffer0, [.unstable])
registerBenchmark("ExistentialTestOneMethodCall_IntValueBuffer1", run_ExistentialTestOneMethodCall_IntValueBuffer1, [.unstable])
registerBenchmark("ExistentialTestOneMethodCall_IntValueBuffer2", run_ExistentialTestOneMethodCall_IntValueBuffer2, [.unstable])
registerBenchmark("ExistentialTestOneMethodCall_IntValueBuffer3", run_ExistentialTestOneMethodCall_IntValueBuffer3, [.unstable])
registerBenchmark("ExistentialTestOneMethodCall_IntValueBuffer4", run_ExistentialTestOneMethodCall_IntValueBuffer4, [.unstable])
registerBenchmark("ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer1", run_ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer1, [.unstable])
registerBenchmark("ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer2", run_ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer2, [.unstable])
registerBenchmark("ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer3", run_ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer3, [.unstable])
registerBenchmark("ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer4", run_ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer4, [.unstable])
registerBenchmark("ExistentialTestPassExistentialOneMethodCall_IntValueBuffer0", run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer0, [.unstable])
registerBenchmark("ExistentialTestPassExistentialOneMethodCall_IntValueBuffer1", run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer1, [.unstable])
registerBenchmark("ExistentialTestPassExistentialOneMethodCall_IntValueBuffer2", run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer2, [.unstable])
registerBenchmark("ExistentialTestPassExistentialOneMethodCall_IntValueBuffer3", run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer3, [.unstable])
registerBenchmark("ExistentialTestPassExistentialOneMethodCall_IntValueBuffer4", run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer4, [.unstable])
registerBenchmark("ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer1", run_ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer1, [.unstable])
registerBenchmark("ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer2", run_ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer2, [.unstable])
registerBenchmark("ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer3", run_ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer3, [.unstable])
registerBenchmark("ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer4", run_ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer4, [.unstable])
registerBenchmark("ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer0", run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer0, [.unstable])
registerBenchmark("ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer1", run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer1, [.unstable])
registerBenchmark("ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer2", run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer2, [.unstable])
registerBenchmark("ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer3", run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer3, [.unstable])
registerBenchmark("ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer4", run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer4, [.unstable])
registerBenchmark("ExistentialTestTwoMethodCalls_ClassValueBuffer1", run_ExistentialTestTwoMethodCalls_ClassValueBuffer1, [.unstable])
registerBenchmark("ExistentialTestTwoMethodCalls_ClassValueBuffer2", run_ExistentialTestTwoMethodCalls_ClassValueBuffer2, [.unstable])
registerBenchmark("ExistentialTestTwoMethodCalls_ClassValueBuffer3", run_ExistentialTestTwoMethodCalls_ClassValueBuffer3, [.unstable])
registerBenchmark("ExistentialTestTwoMethodCalls_ClassValueBuffer4", run_ExistentialTestTwoMethodCalls_ClassValueBuffer4, [.unstable])
registerBenchmark("ExistentialTestTwoMethodCalls_IntValueBuffer0", run_ExistentialTestTwoMethodCalls_IntValueBuffer0, [.unstable])
registerBenchmark("ExistentialTestTwoMethodCalls_IntValueBuffer1", run_ExistentialTestTwoMethodCalls_IntValueBuffer1, [.unstable])
registerBenchmark("ExistentialTestTwoMethodCalls_IntValueBuffer2", run_ExistentialTestTwoMethodCalls_IntValueBuffer2, [.unstable])
registerBenchmark("ExistentialTestTwoMethodCalls_IntValueBuffer3", run_ExistentialTestTwoMethodCalls_IntValueBuffer3, [.unstable])
registerBenchmark("ExistentialTestTwoMethodCalls_IntValueBuffer4", run_ExistentialTestTwoMethodCalls_IntValueBuffer4, [.unstable])
registerBenchmark("Fibonacci", run_Fibonacci, [.unstable, .algorithm])
registerBenchmark("HashQuadratic", run_HashQuadratic, [.unstable, .api, .Dictionary])

// String tests, an extended benchmark suite exercising finer-granularity
// behavior of our Strings.
registerBenchmark("StringWalk_ascii_characters", run_StringWalk_ascii_characters, [.api, .String, .skip])
registerBenchmark("StringWalk_ascii_characters_Backwards", run_StringWalk_ascii_characters_Backwards, [.api, .String, .skip])
registerBenchmark("StringWalk_ascii_unicodeScalars", run_StringWalk_ascii_unicodeScalars, [.api, .String, .skip])
registerBenchmark("StringWalk_ascii_unicodeScalars_Backwards", run_StringWalk_ascii_unicodeScalars_Backwards, [.api, .String, .skip])
registerBenchmark("StringWalk_chinese_characters", run_StringWalk_chinese_characters, [.api, .String, .skip])
registerBenchmark("StringWalk_chinese_characters_Backwards", run_StringWalk_chinese_characters_Backwards, [.api, .String, .skip])
registerBenchmark("StringWalk_chinese_unicodeScalars", run_StringWalk_chinese_unicodeScalars, [.api, .String, .skip])
registerBenchmark("StringWalk_chinese_unicodeScalars_Backwards", run_StringWalk_chinese_unicodeScalars_Backwards, [.api, .String, .skip])
registerBenchmark("StringWalk_japanese_characters", run_StringWalk_japanese_characters, [.api, .String, .skip])
registerBenchmark("StringWalk_japanese_characters_Backwards", run_StringWalk_japanese_characters_Backwards, [.api, .String, .skip])
registerBenchmark("StringWalk_japanese_unicodeScalars", run_StringWalk_japanese_unicodeScalars, [.api, .String, .skip])
registerBenchmark("StringWalk_japanese_unicodeScalars_Backwards", run_StringWalk_japanese_unicodeScalars_Backwards, [.api, .String, .skip])
registerBenchmark("StringWalk_korean_characters", run_StringWalk_korean_characters, [.api, .String, .skip])
registerBenchmark("StringWalk_korean_characters_Backwards", run_StringWalk_korean_characters_Backwards, [.api, .String, .skip])
registerBenchmark("StringWalk_korean_unicodeScalars", run_StringWalk_korean_unicodeScalars, [.api, .String, .skip])
registerBenchmark("StringWalk_korean_unicodeScalars_Backwards", run_StringWalk_korean_unicodeScalars_Backwards, [.api, .String, .skip])
registerBenchmark("StringWalk_punctuatedJapanese_characters", run_StringWalk_punctuatedJapanese_characters, [.api, .String, .skip])
registerBenchmark("StringWalk_punctuatedJapanese_characters_Backwards", run_StringWalk_punctuatedJapanese_characters_Backwards, [.api, .String, .skip])
registerBenchmark("StringWalk_punctuatedJapanese_unicodeScalars", run_StringWalk_punctuatedJapanese_unicodeScalars, [.api, .String, .skip])
registerBenchmark("StringWalk_punctuatedJapanese_unicodeScalars_Backwards", run_StringWalk_punctuatedJapanese_unicodeScalars_Backwards, [.api, .String, .skip])
registerBenchmark("StringWalk_punctuated_characters", run_StringWalk_punctuated_characters, [.api, .String, .skip])
registerBenchmark("StringWalk_punctuated_characters_Backwards", run_StringWalk_punctuated_characters_Backwards, [.api, .String, .skip])
registerBenchmark("StringWalk_punctuated_unicodeScalars", run_StringWalk_punctuated_unicodeScalars, [.api, .String, .skip])
registerBenchmark("StringWalk_punctuated_unicodeScalars_Backwards", run_StringWalk_punctuated_unicodeScalars_Backwards, [.api, .String, .skip])
registerBenchmark("StringWalk_russian_characters", run_StringWalk_russian_characters, [.api, .String, .skip])
registerBenchmark("StringWalk_russian_characters_Backwards", run_StringWalk_russian_characters_Backwards, [.api, .String, .skip])
registerBenchmark("StringWalk_russian_unicodeScalars", run_StringWalk_russian_unicodeScalars, [.api, .String, .skip])
registerBenchmark("StringWalk_russian_unicodeScalars_Backwards", run_StringWalk_russian_unicodeScalars_Backwards, [.api, .String, .skip])
registerBenchmark("StringWalk_tweet_characters", run_StringWalk_tweet_characters, [.api, .String, .skip])
registerBenchmark("StringWalk_tweet_characters_Backwards", run_StringWalk_tweet_characters_Backwards, [.api, .String, .skip])
registerBenchmark("StringWalk_tweet_unicodeScalars", run_StringWalk_tweet_unicodeScalars, [.api, .String, .skip])
registerBenchmark("StringWalk_tweet_unicodeScalars_Backwards", run_StringWalk_tweet_unicodeScalars_Backwards, [.api, .String, .skip])
registerBenchmark("StringWalk_utf16_characters", run_StringWalk_utf16_characters, [.api, .String, .skip])
registerBenchmark("StringWalk_utf16_characters_Backwards", run_StringWalk_utf16_characters_Backwards, [.api, .String, .skip])
registerBenchmark("StringWalk_utf16_unicodeScalars", run_StringWalk_utf16_unicodeScalars, [.api, .String, .skip])
registerBenchmark("StringWalk_utf16_unicodeScalars_Backwards", run_StringWalk_utf16_unicodeScalars_Backwards, [.api, .String, .skip])

main()
