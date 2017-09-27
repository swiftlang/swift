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

registerBenchmark(AnyHashableWithAClass)
registerBenchmark(ArraySetElement)
registerBenchmark(ExclusivityGlobal)
registerBenchmark(ExclusivityInMatSet)
registerBenchmark(ExclusivityIndependent)
registerBenchmark(LinkedList)
registerBenchmark(ObjectAllocation)
registerBenchmark(PolymorphicCalls)
registerBenchmark(SevenBoom)

@inline(__always)
private func addTo(
  _ testSuite: inout [String : ((Int) -> (), [BenchmarkCategory])],
  _ name: String,
  _ function: @escaping (Int) -> (),
  _ tags: [BenchmarkCategory] = []
  ) {
  testSuite[name] = (function, tags)
}

// The main test suite: precommit tests
addTo(&precommitTests, "AngryPhonebook", run_AngryPhonebook, [.validation, .api, .String])
addTo(&precommitTests, "AnyHashableWithAClass", run_AnyHashableWithAClass, [.validation, .abstraction, .runtime])
addTo(&precommitTests, "Array2D", run_Array2D, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayAppend", run_ArrayAppend, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayAppendArrayOfInt", run_ArrayAppendArrayOfInt, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayAppendAscii", run_ArrayAppendAscii, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayAppendFromGeneric", run_ArrayAppendFromGeneric, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayAppendGenericStructs", run_ArrayAppendGenericStructs, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayAppendLatin1", run_ArrayAppendLatin1, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayAppendLazyMap", run_ArrayAppendLazyMap, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayAppendOptionals", run_ArrayAppendOptionals, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayAppendRepeatCol", run_ArrayAppendRepeatCol, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayAppendReserved", run_ArrayAppendReserved, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayAppendSequence", run_ArrayAppendSequence, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayAppendStrings", run_ArrayAppendStrings, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayAppendToFromGeneric", run_ArrayAppendToFromGeneric, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayAppendToGeneric", run_ArrayAppendToGeneric, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayAppendUTF16", run_ArrayAppendUTF16, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayInClass", run_ArrayInClass, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayLiteral", run_ArrayLiteral, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayOfGenericPOD", run_ArrayOfGenericPOD, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayOfGenericRef", run_ArrayOfGenericRef, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayOfPOD", run_ArrayOfPOD, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayOfRef", run_ArrayOfRef, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayPlusEqualArrayOfInt", run_ArrayPlusEqualArrayOfInt, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayPlusEqualFiveElementCollection", run_ArrayPlusEqualFiveElementCollection, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayPlusEqualSingleElementCollection", run_ArrayPlusEqualSingleElementCollection, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayPlusEqualThreeElements", run_ArrayPlusEqualThreeElements, [.validation, .api, .Array])
addTo(&precommitTests, "ArraySubscript", run_ArraySubscript, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayValueProp", run_ArrayValueProp, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayValueProp2", run_ArrayValueProp2, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayValueProp3", run_ArrayValueProp3, [.validation, .api, .Array])
addTo(&precommitTests, "ArrayValueProp4", run_ArrayValueProp4, [.validation, .api, .Array])
addTo(&precommitTests, "BitCount", run_BitCount, [.validation, .algorithm])
addTo(&precommitTests, "ByteSwap", run_ByteSwap, [.validation, .algorithm])
addTo(&precommitTests, "CStringLongAscii", run_CStringLongAscii, [.validation, .api, .String, .bridging])
addTo(&precommitTests, "CStringLongNonAscii", run_CStringLongNonAscii, [.validation, .api, .String, .bridging])
addTo(&precommitTests, "CStringShortAscii", run_CStringShortAscii, [.validation, .api, .String, .bridging])
addTo(&precommitTests, "Calculator", run_Calculator, [.validation])
addTo(&precommitTests, "CaptureProp", run_CaptureProp, [.validation, .api, .refcount])
addTo(&precommitTests, "CharIndexing_ascii_unicodeScalars", run_CharIndexing_ascii_unicodeScalars, [.validation, .api, .String])
addTo(&precommitTests, "CharIndexing_ascii_unicodeScalars_Backwards", run_CharIndexing_ascii_unicodeScalars_Backwards, [.validation, .api, .String])
addTo(&precommitTests, "CharIndexing_chinese_unicodeScalars", run_CharIndexing_chinese_unicodeScalars, [.validation, .api, .String])
addTo(&precommitTests, "CharIndexing_chinese_unicodeScalars_Backwards", run_CharIndexing_chinese_unicodeScalars_Backwards, [.validation, .api, .String])
addTo(&precommitTests, "CharIndexing_japanese_unicodeScalars", run_CharIndexing_japanese_unicodeScalars, [.validation, .api, .String])
addTo(&precommitTests, "CharIndexing_japanese_unicodeScalars_Backwards", run_CharIndexing_japanese_unicodeScalars_Backwards, [.validation, .api, .String])
addTo(&precommitTests, "CharIndexing_korean_unicodeScalars", run_CharIndexing_korean_unicodeScalars, [.validation, .api, .String])
addTo(&precommitTests, "CharIndexing_korean_unicodeScalars_Backwards", run_CharIndexing_korean_unicodeScalars_Backwards, [.validation, .api, .String])
addTo(&precommitTests, "CharIndexing_punctuatedJapanese_unicodeScalars", run_CharIndexing_punctuatedJapanese_unicodeScalars, [.validation, .api, .String])
addTo(&precommitTests, "CharIndexing_punctuatedJapanese_unicodeScalars_Backwards", run_CharIndexing_punctuatedJapanese_unicodeScalars_Backwards, [.validation, .api, .String])
addTo(&precommitTests, "CharIndexing_punctuated_unicodeScalars", run_CharIndexing_punctuated_unicodeScalars, [.validation, .api, .String])
addTo(&precommitTests, "CharIndexing_punctuated_unicodeScalars_Backwards", run_CharIndexing_punctuated_unicodeScalars_Backwards, [.validation, .api, .String])
addTo(&precommitTests, "CharIndexing_russian_unicodeScalars", run_CharIndexing_russian_unicodeScalars, [.validation, .api, .String])
addTo(&precommitTests, "CharIndexing_russian_unicodeScalars_Backwards", run_CharIndexing_russian_unicodeScalars_Backwards, [.validation, .api, .String])
addTo(&precommitTests, "CharIndexing_tweet_unicodeScalars", run_CharIndexing_tweet_unicodeScalars, [.validation, .api, .String])
addTo(&precommitTests, "CharIndexing_tweet_unicodeScalars_Backwards", run_CharIndexing_tweet_unicodeScalars_Backwards, [.validation, .api, .String])
addTo(&precommitTests, "CharIndexing_utf16_unicodeScalars", run_CharIndexing_utf16_unicodeScalars, [.validation, .api, .String])
addTo(&precommitTests, "CharIndexing_utf16_unicodeScalars_Backwards", run_CharIndexing_utf16_unicodeScalars_Backwards, [.validation, .api, .String])
addTo(&precommitTests, "CharIteration_ascii_unicodeScalars", run_CharIteration_ascii_unicodeScalars, [.validation, .api, .String])
addTo(&precommitTests, "CharIteration_ascii_unicodeScalars_Backwards", run_CharIteration_ascii_unicodeScalars_Backwards, [.validation, .api, .String])
addTo(&precommitTests, "CharIteration_chinese_unicodeScalars", run_CharIteration_chinese_unicodeScalars, [.validation, .api, .String])
addTo(&precommitTests, "CharIteration_chinese_unicodeScalars_Backwards", run_CharIteration_chinese_unicodeScalars_Backwards, [.validation, .api, .String])
addTo(&precommitTests, "CharIteration_japanese_unicodeScalars", run_CharIteration_japanese_unicodeScalars, [.validation, .api, .String])
addTo(&precommitTests, "CharIteration_japanese_unicodeScalars_Backwards", run_CharIteration_japanese_unicodeScalars_Backwards, [.validation, .api, .String])
addTo(&precommitTests, "CharIteration_korean_unicodeScalars", run_CharIteration_korean_unicodeScalars, [.validation, .api, .String])
addTo(&precommitTests, "CharIteration_korean_unicodeScalars_Backwards", run_CharIteration_korean_unicodeScalars_Backwards, [.validation, .api, .String])
addTo(&precommitTests, "CharIteration_punctuatedJapanese_unicodeScalars", run_CharIteration_punctuatedJapanese_unicodeScalars, [.validation, .api, .String])
addTo(&precommitTests, "CharIteration_punctuatedJapanese_unicodeScalars_Backwards", run_CharIteration_punctuatedJapanese_unicodeScalars_Backwards, [.validation, .api, .String])
addTo(&precommitTests, "CharIteration_punctuated_unicodeScalars", run_CharIteration_punctuated_unicodeScalars, [.validation, .api, .String])
addTo(&precommitTests, "CharIteration_punctuated_unicodeScalars_Backwards", run_CharIteration_punctuated_unicodeScalars_Backwards, [.validation, .api, .String])
addTo(&precommitTests, "CharIteration_russian_unicodeScalars", run_CharIteration_russian_unicodeScalars, [.validation, .api, .String])
addTo(&precommitTests, "CharIteration_russian_unicodeScalars_Backwards", run_CharIteration_russian_unicodeScalars_Backwards, [.validation, .api, .String])
addTo(&precommitTests, "CharIteration_tweet_unicodeScalars", run_CharIteration_tweet_unicodeScalars, [.validation, .api, .String])
addTo(&precommitTests, "CharIteration_tweet_unicodeScalars_Backwards", run_CharIteration_tweet_unicodeScalars_Backwards, [.validation, .api, .String])
addTo(&precommitTests, "CharIteration_utf16_unicodeScalars", run_CharIteration_utf16_unicodeScalars, [.validation, .api, .String])
addTo(&precommitTests, "CharIteration_utf16_unicodeScalars_Backwards", run_CharIteration_utf16_unicodeScalars_Backwards, [.validation, .api, .String])
addTo(&precommitTests, "CharacterLiteralsLarge", run_CharacterLiteralsLarge, [.validation, .api, .String])
addTo(&precommitTests, "CharacterLiteralsSmall", run_CharacterLiteralsSmall, [.validation, .api, .String])
addTo(&precommitTests, "Chars", run_Chars, [.validation, .api, .String])
addTo(&precommitTests, "ClassArrayGetter", run_ClassArrayGetter, [.validation, .api, .Array])
addTo(&precommitTests, "DeadArray", run_DeadArray, [.regression])
addTo(&precommitTests, "Dictionary", run_Dictionary, [.validation, .api, .Dictionary])
addTo(&precommitTests, "Dictionary2", run_Dictionary2, [.validation, .api, .Dictionary])
addTo(&precommitTests, "Dictionary2OfObjects", run_Dictionary2OfObjects, [.validation, .api, .Dictionary])
addTo(&precommitTests, "Dictionary3", run_Dictionary3, [.validation, .api, .Dictionary])
addTo(&precommitTests, "Dictionary3OfObjects", run_Dictionary3OfObjects, [.validation, .api, .Dictionary])
addTo(&precommitTests, "DictionaryBridge", run_DictionaryBridge, [.validation, .api, .Dictionary, .bridging])
addTo(&precommitTests, "DictionaryGroup", run_DictionaryGroup, [.validation, .api, .Dictionary])
addTo(&precommitTests, "DictionaryGroupOfObjects", run_DictionaryGroupOfObjects, [.validation, .api, .Dictionary])
addTo(&precommitTests, "DictionaryLiteral", run_DictionaryLiteral, [.validation, .api, .Dictionary])
addTo(&precommitTests, "DictionaryOfObjects", run_DictionaryOfObjects, [.validation, .api, .Dictionary])
addTo(&precommitTests, "DictionaryRemove", run_DictionaryRemove, [.validation, .api, .Dictionary])
addTo(&precommitTests, "DictionaryRemoveOfObjects", run_DictionaryRemoveOfObjects, [.validation, .api, .Dictionary])
addTo(&precommitTests, "DictionarySwap", run_DictionarySwap, [.validation, .api, .Dictionary])
addTo(&precommitTests, "DictionarySwapOfObjects", run_DictionarySwapOfObjects, [.validation, .api, .Dictionary])
addTo(&precommitTests, "DropFirstAnyCollection", run_DropFirstAnyCollection, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropFirstAnyCollectionLazy", run_DropFirstAnyCollectionLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropFirstAnySeqCRangeIter", run_DropFirstAnySeqCRangeIter, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropFirstAnySeqCRangeIterLazy", run_DropFirstAnySeqCRangeIterLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropFirstAnySeqCntRange", run_DropFirstAnySeqCntRange, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropFirstAnySeqCntRangeLazy", run_DropFirstAnySeqCntRangeLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropFirstAnySequence", run_DropFirstAnySequence, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropFirstAnySequenceLazy", run_DropFirstAnySequenceLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropFirstArray", run_DropFirstArray, [.validation, .api, .Array])
addTo(&precommitTests, "DropFirstArrayLazy", run_DropFirstArrayLazy, [.validation, .api, .Array])
addTo(&precommitTests, "DropFirstCountableRange", run_DropFirstCountableRange, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropFirstCountableRangeLazy", run_DropFirstCountableRangeLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropFirstSequence", run_DropFirstSequence, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropFirstSequenceLazy", run_DropFirstSequenceLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropLastAnyCollection", run_DropLastAnyCollection, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropLastAnyCollectionLazy", run_DropLastAnyCollectionLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropLastAnySeqCRangeIter", run_DropLastAnySeqCRangeIter, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropLastAnySeqCRangeIterLazy", run_DropLastAnySeqCRangeIterLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropLastAnySeqCntRange", run_DropLastAnySeqCntRange, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropLastAnySeqCntRangeLazy", run_DropLastAnySeqCntRangeLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropLastAnySequence", run_DropLastAnySequence, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropLastAnySequenceLazy", run_DropLastAnySequenceLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropLastArray", run_DropLastArray, [.validation, .api, .Array])
addTo(&precommitTests, "DropLastArrayLazy", run_DropLastArrayLazy, [.validation, .api, .Array])
addTo(&precommitTests, "DropLastCountableRange", run_DropLastCountableRange, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropLastCountableRangeLazy", run_DropLastCountableRangeLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropLastSequence", run_DropLastSequence, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropLastSequenceLazy", run_DropLastSequenceLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropWhileAnyCollection", run_DropWhileAnyCollection, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropWhileAnyCollectionLazy", run_DropWhileAnyCollectionLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropWhileAnySeqCRangeIter", run_DropWhileAnySeqCRangeIter, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropWhileAnySeqCRangeIterLazy", run_DropWhileAnySeqCRangeIterLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropWhileAnySeqCntRange", run_DropWhileAnySeqCntRange, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropWhileAnySeqCntRangeLazy", run_DropWhileAnySeqCntRangeLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropWhileAnySequence", run_DropWhileAnySequence, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropWhileAnySequenceLazy", run_DropWhileAnySequenceLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropWhileArray", run_DropWhileArray, [.validation, .api, .Array])
addTo(&precommitTests, "DropWhileArrayLazy", run_DropWhileArrayLazy, [.validation, .api, .Array])
addTo(&precommitTests, "DropWhileCountableRange", run_DropWhileCountableRange, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropWhileCountableRangeLazy", run_DropWhileCountableRangeLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropWhileSequence", run_DropWhileSequence, [.validation, .api, .abstraction])
addTo(&precommitTests, "DropWhileSequenceLazy", run_DropWhileSequenceLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "EqualStringSubstring", run_EqualStringSubstring, [.validation, .api, .String])
addTo(&precommitTests, "EqualSubstringString", run_EqualSubstringString, [.validation, .api, .String])
addTo(&precommitTests, "EqualSubstringSubstring", run_EqualSubstringSubstring, [.validation, .api, .String])
addTo(&precommitTests, "EqualSubstringSubstringGenericEquatable", run_EqualSubstringSubstringGenericEquatable, [.validation, .api, .String, .abstraction])
addTo(&precommitTests, "ErrorHandling", run_ErrorHandling, [.validation, .exceptions])
addTo(&precommitTests, "FilterEvenUsingReduce", run_FilterEvenUsingReduce, [.validation, .api])
addTo(&precommitTests, "FilterEvenUsingReduceInto", run_FilterEvenUsingReduceInto, [.validation, .api])
addTo(&precommitTests, "FrequenciesUsingReduce", run_FrequenciesUsingReduce, [.validation, .api])
addTo(&precommitTests, "FrequenciesUsingReduceInto", run_FrequenciesUsingReduceInto, [.validation, .api])
addTo(&precommitTests, "Hanoi", run_Hanoi, [.validation, .algorithm])
addTo(&precommitTests, "HashTest", run_HashTest, [.validation, .algorithm])
addTo(&precommitTests, "Histogram", run_Histogram, [.validation, .algorithm])
addTo(&precommitTests, "Integrate", run_Integrate, [.validation, .algorithm])
addTo(&precommitTests, "IterateData", run_IterateData, [.validation, .api])
addTo(&precommitTests, "Join", run_Join, [.validation, .api, .String, .Array])
addTo(&precommitTests, "LazilyFilteredArrays", run_LazilyFilteredArrays, [.validation, .api, .Array])
addTo(&precommitTests, "LazilyFilteredRange", run_LazilyFilteredRange, [.validation, .api, .Array])
addTo(&precommitTests, "LessSubstringSubstring", run_LessSubstringSubstring, [.validation, .api, .String])
addTo(&precommitTests, "LessSubstringSubstringGenericComparable", run_LessSubstringSubstringGenericComparable, [.validation, .api, .String, .abstraction])
addTo(&precommitTests, "LinkedList", run_LinkedList, [.validation])
addTo(&precommitTests, "MapReduce", run_MapReduce, [.validation, .algorithm])
addTo(&precommitTests, "MapReduceAnyCollection", run_MapReduceAnyCollection, [.validation, .algorithm, .abstraction])
addTo(&precommitTests, "MapReduceAnyCollectionShort", run_MapReduceAnyCollectionShort, [.validation, .algorithm, .abstraction])
addTo(&precommitTests, "MapReduceClass", run_MapReduceClass, [.validation, .algorithm, .abstraction])
addTo(&precommitTests, "MapReduceClassShort", run_MapReduceClassShort, [.validation, .algorithm, .abstraction])
addTo(&precommitTests, "MapReduceLazyCollection", run_MapReduceLazyCollection, [.validation, .algorithm, .abstraction])
addTo(&precommitTests, "MapReduceLazyCollectionShort", run_MapReduceLazyCollectionShort, [.validation, .algorithm, .abstraction])
addTo(&precommitTests, "MapReduceLazySequence", run_MapReduceLazySequence, [.validation, .algorithm, .abstraction])
addTo(&precommitTests, "MapReduceSequence", run_MapReduceSequence, [.validation, .algorithm, .abstraction])
addTo(&precommitTests, "MapReduceShort", run_MapReduceShort, [.validation, .algorithm, .abstraction])
addTo(&precommitTests, "MapReduceShortString", run_MapReduceShortString, [.validation, .algorithm, .String])
addTo(&precommitTests, "MapReduceString", run_MapReduceString, [.validation, .algorithm, .String])
addTo(&precommitTests, "Memset", run_Memset, [.validation])
addTo(&precommitTests, "MonteCarloE", run_MonteCarloE, [.validation, .algorithm])
addTo(&precommitTests, "MonteCarloPi", run_MonteCarloPi, [.validation, .algorithm])
addTo(&precommitTests, "NSDictionaryCastToSwift", run_NSDictionaryCastToSwift, [.validation, .api, .Dictionary, .bridging])
addTo(&precommitTests, "NSError", run_NSError, [.validation, .exceptions, .bridging])
addTo(&precommitTests, "NSStringConversion", run_NSStringConversion, [.validation, .api, .String, .bridging])
addTo(&precommitTests, "NopDeinit", run_NopDeinit, [.regression])
addTo(&precommitTests, "ObjectAllocation", run_ObjectAllocation, [.validation])
addTo(&precommitTests, "ObjectiveCBridgeFromNSArrayAnyObject", run_ObjectiveCBridgeFromNSArrayAnyObject, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeFromNSArrayAnyObjectForced", run_ObjectiveCBridgeFromNSArrayAnyObjectForced, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeFromNSArrayAnyObjectToString", run_ObjectiveCBridgeFromNSArrayAnyObjectToString, [.validation, .bridging, .String])
addTo(&precommitTests, "ObjectiveCBridgeFromNSArrayAnyObjectToStringForced", run_ObjectiveCBridgeFromNSArrayAnyObjectToStringForced, [.validation, .bridging, .String])
addTo(&precommitTests, "ObjectiveCBridgeFromNSDictionaryAnyObject", run_ObjectiveCBridgeFromNSDictionaryAnyObject, [.validation, .bridging, .abstraction])
addTo(&precommitTests, "ObjectiveCBridgeFromNSDictionaryAnyObjectForced", run_ObjectiveCBridgeFromNSDictionaryAnyObjectForced, [.validation, .bridging, .abstraction])
addTo(&precommitTests, "ObjectiveCBridgeFromNSDictionaryAnyObjectToString", run_ObjectiveCBridgeFromNSDictionaryAnyObjectToString, [.validation, .bridging, .abstraction, .String])
addTo(&precommitTests, "ObjectiveCBridgeFromNSDictionaryAnyObjectToStringForced", run_ObjectiveCBridgeFromNSDictionaryAnyObjectToStringForced, [.validation, .bridging, .String])
addTo(&precommitTests, "ObjectiveCBridgeFromNSSetAnyObject", run_ObjectiveCBridgeFromNSSetAnyObject, [.validation, .bridging, .abstraction])
addTo(&precommitTests, "ObjectiveCBridgeFromNSSetAnyObjectForced", run_ObjectiveCBridgeFromNSSetAnyObjectForced, [.validation, .bridging, .abstraction])
addTo(&precommitTests, "ObjectiveCBridgeFromNSSetAnyObjectToString", run_ObjectiveCBridgeFromNSSetAnyObjectToString, [.validation, .bridging, .String])
addTo(&precommitTests, "ObjectiveCBridgeFromNSSetAnyObjectToStringForced", run_ObjectiveCBridgeFromNSSetAnyObjectToStringForced, [.validation, .bridging, .String])
addTo(&precommitTests, "ObjectiveCBridgeFromNSString", run_ObjectiveCBridgeFromNSString, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeFromNSStringForced", run_ObjectiveCBridgeFromNSStringForced, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeStubDataAppend", run_ObjectiveCBridgeStubDataAppend, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeStubDateAccess", run_ObjectiveCBridgeStubDateAccess, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeStubDateMutation", run_ObjectiveCBridgeStubDateMutation, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeStubFromArrayOfNSString", run_ObjectiveCBridgeStubFromArrayOfNSString, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeStubFromNSDate", run_ObjectiveCBridgeStubFromNSDate, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeStubFromNSDateRef", run_ObjectiveCBridgeStubFromNSDateRef, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeStubFromNSString", run_ObjectiveCBridgeStubFromNSString, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeStubFromNSStringRef", run_ObjectiveCBridgeStubFromNSStringRef, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeStubNSDataAppend", run_ObjectiveCBridgeStubNSDataAppend, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeStubNSDateMutationRef", run_ObjectiveCBridgeStubNSDateMutationRef, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeStubNSDateRefAccess", run_ObjectiveCBridgeStubNSDateRefAccess, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeStubToArrayOfNSString", run_ObjectiveCBridgeStubToArrayOfNSString, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeStubToNSDate", run_ObjectiveCBridgeStubToNSDate, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeStubToNSDateRef", run_ObjectiveCBridgeStubToNSDateRef, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeStubToNSString", run_ObjectiveCBridgeStubToNSString, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeStubToNSStringRef", run_ObjectiveCBridgeStubToNSStringRef, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeStubURLAppendPath", run_ObjectiveCBridgeStubURLAppendPath, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeStubURLAppendPathRef", run_ObjectiveCBridgeStubURLAppendPathRef, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeToNSArray", run_ObjectiveCBridgeToNSArray, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeToNSDictionary", run_ObjectiveCBridgeToNSDictionary, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeToNSSet", run_ObjectiveCBridgeToNSSet, [.validation, .bridging])
addTo(&precommitTests, "ObjectiveCBridgeToNSString", run_ObjectiveCBridgeToNSString, [.validation, .bridging])
addTo(&precommitTests, "ObserverClosure", run_ObserverClosure, [.validation])
addTo(&precommitTests, "ObserverForwarderStruct", run_ObserverForwarderStruct, [.validation])
addTo(&precommitTests, "ObserverPartiallyAppliedMethod", run_ObserverPartiallyAppliedMethod, [.validation])
addTo(&precommitTests, "ObserverUnappliedMethod", run_ObserverUnappliedMethod, [.validation])
addTo(&precommitTests, "OpenClose", run_OpenClose, [.validation, .api, .String])
addTo(&precommitTests, "Phonebook", run_Phonebook, [.validation, .api, .String])
addTo(&precommitTests, "PolymorphicCalls", run_PolymorphicCalls, [.validation, .runtime, .abstraction])
addTo(&precommitTests, "PopFrontArray", run_PopFrontArray, [.validation, .api, .Array])
addTo(&precommitTests, "PopFrontArrayGeneric", run_PopFrontArrayGeneric, [.validation, .api, .Array, .abstraction])
addTo(&precommitTests, "PopFrontUnsafePointer", run_PopFrontUnsafePointer, [.validation, .api])
addTo(&precommitTests, "PrefixAnyCollection", run_PrefixAnyCollection, [.validation, .api, .abstraction])
addTo(&precommitTests, "PrefixAnyCollectionLazy", run_PrefixAnyCollectionLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "PrefixAnySeqCRangeIter", run_PrefixAnySeqCRangeIter, [.validation, .api, .abstraction])
addTo(&precommitTests, "PrefixAnySeqCRangeIterLazy", run_PrefixAnySeqCRangeIterLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "PrefixAnySeqCntRange", run_PrefixAnySeqCntRange, [.validation, .api, .abstraction])
addTo(&precommitTests, "PrefixAnySeqCntRangeLazy", run_PrefixAnySeqCntRangeLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "PrefixAnySequence", run_PrefixAnySequence, [.validation, .api, .abstraction])
addTo(&precommitTests, "PrefixAnySequenceLazy", run_PrefixAnySequenceLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "PrefixArray", run_PrefixArray, [.validation, .api, .Array])
addTo(&precommitTests, "PrefixArrayLazy", run_PrefixArrayLazy, [.validation, .api, .Array])
addTo(&precommitTests, "PrefixCountableRange", run_PrefixCountableRange, [.validation, .api, .abstraction])
addTo(&precommitTests, "PrefixCountableRangeLazy", run_PrefixCountableRangeLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "PrefixSequence", run_PrefixSequence, [.validation, .api, .abstraction])
addTo(&precommitTests, "PrefixSequenceLazy", run_PrefixSequenceLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "PrefixWhileAnyCollection", run_PrefixWhileAnyCollection, [.validation, .api, .abstraction])
addTo(&precommitTests, "PrefixWhileAnyCollectionLazy", run_PrefixWhileAnyCollectionLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "PrefixWhileAnySeqCRangeIter", run_PrefixWhileAnySeqCRangeIter, [.validation, .api, .abstraction])
addTo(&precommitTests, "PrefixWhileAnySeqCRangeIterLazy", run_PrefixWhileAnySeqCRangeIterLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "PrefixWhileAnySeqCntRange", run_PrefixWhileAnySeqCntRange, [.validation, .api, .abstraction])
addTo(&precommitTests, "PrefixWhileAnySeqCntRangeLazy", run_PrefixWhileAnySeqCntRangeLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "PrefixWhileAnySequence", run_PrefixWhileAnySequence, [.validation, .api, .abstraction])
addTo(&precommitTests, "PrefixWhileAnySequenceLazy", run_PrefixWhileAnySequenceLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "PrefixWhileArray", run_PrefixWhileArray, [.validation, .api, .Array])
addTo(&precommitTests, "PrefixWhileArrayLazy", run_PrefixWhileArrayLazy, [.validation, .api, .Array])
addTo(&precommitTests, "PrefixWhileCountableRange", run_PrefixWhileCountableRange, [.validation, .api, .abstraction])
addTo(&precommitTests, "PrefixWhileCountableRangeLazy", run_PrefixWhileCountableRangeLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "PrefixWhileSequence", run_PrefixWhileSequence, [.validation, .api, .abstraction])
addTo(&precommitTests, "PrefixWhileSequenceLazy", run_PrefixWhileSequenceLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "Prims", run_Prims, [.validation, .algorithm])
addTo(&precommitTests, "PrimsSplit", run_PrimsSplit, [.validation, .algorithm])
addTo(&precommitTests, "ProtocolDispatch", run_ProtocolDispatch, [.validation, .abstraction])
addTo(&precommitTests, "ProtocolDispatch2", run_ProtocolDispatch2, [.validation, .abstraction])
addTo(&precommitTests, "RC4", run_RC4, [.validation, .algorithm])
addTo(&precommitTests, "RGBHistogram", run_RGBHistogram, [.validation, .algorithm])
addTo(&precommitTests, "RGBHistogramOfObjects", run_RGBHistogramOfObjects, [.validation, .algorithm])
addTo(&precommitTests, "RangeAssignment", run_RangeAssignment, [.validation, .api, .abstraction])
addTo(&precommitTests, "RecursiveOwnedParameter", run_RecursiveOwnedParameter, [.validation, .api, .Array, .refcount])
addTo(&precommitTests, "ReversedArray", run_ReversedArray, [.validation, .api, .Array])
addTo(&precommitTests, "ReversedBidirectional", run_ReversedBidirectional, [.validation, .api, .abstraction])
addTo(&precommitTests, "ReversedDictionary", run_ReversedDictionary, [.validation, .api, .Dictionary])
addTo(&precommitTests, "SetExclusiveOr", run_SetExclusiveOr, [.validation, .api, .Set])
addTo(&precommitTests, "SetExclusiveOr_OfObjects", run_SetExclusiveOr_OfObjects, [.validation, .api, .Set])
addTo(&precommitTests, "SetIntersect", run_SetIntersect, [.validation, .api, .Set])
addTo(&precommitTests, "SetIntersect_OfObjects", run_SetIntersect_OfObjects, [.validation, .api, .Set])
addTo(&precommitTests, "SetIsSubsetOf", run_SetIsSubsetOf, [.validation, .api, .Set])
addTo(&precommitTests, "SetIsSubsetOf_OfObjects", run_SetIsSubsetOf_OfObjects, [.validation, .api, .Set])
addTo(&precommitTests, "SetUnion", run_SetUnion, [.validation, .api, .Set])
addTo(&precommitTests, "SetUnion_OfObjects", run_SetUnion_OfObjects, [.validation, .api, .Set])
addTo(&precommitTests, "SevenBoom", run_SevenBoom, [.validation])
addTo(&precommitTests, "Sim2DArray", run_Sim2DArray, [.validation, .api, .Array])
addTo(&precommitTests, "SortLargeExistentials", run_SortLargeExistentials, [.validation, .api, .algorithm])
addTo(&precommitTests, "SortLettersInPlace", run_SortLettersInPlace, [.validation, .api, .algorithm, .String])
addTo(&precommitTests, "SortSortedStrings", run_SortSortedStrings, [.validation, .api, .algorithm, .String])
addTo(&precommitTests, "SortStrings", run_SortStrings, [.validation, .api, .algorithm, .String])
addTo(&precommitTests, "SortStringsUnicode", run_SortStringsUnicode, [.validation, .api, .algorithm, .String])
addTo(&precommitTests, "StackPromo", run_StackPromo, [.regression])
addTo(&precommitTests, "StaticArray", run_StaticArray, [.validation, .api, .Array])
addTo(&precommitTests, "StrComplexWalk", run_StrComplexWalk, [.validation, .api, .String])
addTo(&precommitTests, "StrToInt", run_StrToInt, [.validation, .api, .String])
addTo(&precommitTests, "StringAdder", run_StringAdder, [.validation, .api, .String])
addTo(&precommitTests, "StringBuilder", run_StringBuilder, [.validation, .api, .String])
addTo(&precommitTests, "StringBuilderLong", run_StringBuilderLong, [.validation, .api, .String])
addTo(&precommitTests, "StringEdits", run_StringEdits, [.validation, .api, .String])
addTo(&precommitTests, "StringEnumRawValueInitialization", run_StringEnumRawValueInitialization, [.validation, .api, .String])
addTo(&precommitTests, "StringEqualPointerComparison", run_StringEqualPointerComparison, [.validation, .api, .String])
addTo(&precommitTests, "StringFromLongWholeSubstring", run_StringFromLongWholeSubstring, [.validation, .api, .String])
addTo(&precommitTests, "StringFromLongWholeSubstringGeneric", run_StringFromLongWholeSubstringGeneric, [.validation, .api, .String, .abstraction])
addTo(&precommitTests, "StringHasPrefix", run_StringHasPrefix, [.validation, .api, .String])
addTo(&precommitTests, "StringHasPrefixUnicode", run_StringHasPrefixUnicode, [.validation, .api, .String])
addTo(&precommitTests, "StringHasSuffix", run_StringHasSuffix, [.validation, .api, .String])
addTo(&precommitTests, "StringHasSuffixUnicode", run_StringHasSuffixUnicode, [.validation, .api, .String])
addTo(&precommitTests, "StringInterpolation", run_StringInterpolation, [.validation, .api, .String])
addTo(&precommitTests, "StringMatch", run_StringMatch, [.validation, .api, .String])
addTo(&precommitTests, "StringUTF16Builder", run_StringUTF16Builder, [.validation, .api, .String])
addTo(&precommitTests, "StringWalk", run_StringWalk, [.validation, .api, .String])
addTo(&precommitTests, "StringWithCString", run_StringWithCString, [.validation, .api, .String, .bridging])
addTo(&precommitTests, "SubstringComparable", run_SubstringComparable, [.validation, .api, .String])
addTo(&precommitTests, "SubstringEqualString", run_SubstringEqualString, [.validation, .api, .String])
addTo(&precommitTests, "SubstringEquatable", run_SubstringEquatable, [.validation, .api, .String])
addTo(&precommitTests, "SubstringFromLongString", run_SubstringFromLongString, [.validation, .api, .String])
addTo(&precommitTests, "SubstringFromLongStringGeneric", run_SubstringFromLongStringGeneric, [.validation, .api, .String, .abstraction])
addTo(&precommitTests, "SuffixAnyCollection", run_SuffixAnyCollection, [.validation, .api, .abstraction])
addTo(&precommitTests, "SuffixAnyCollectionLazy", run_SuffixAnyCollectionLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "SuffixAnySeqCRangeIter", run_SuffixAnySeqCRangeIter, [.validation, .api, .abstraction])
addTo(&precommitTests, "SuffixAnySeqCRangeIterLazy", run_SuffixAnySeqCRangeIterLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "SuffixAnySeqCntRange", run_SuffixAnySeqCntRange, [.validation, .api, .abstraction])
addTo(&precommitTests, "SuffixAnySeqCntRangeLazy", run_SuffixAnySeqCntRangeLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "SuffixAnySequence", run_SuffixAnySequence, [.validation, .api, .abstraction])
addTo(&precommitTests, "SuffixAnySequenceLazy", run_SuffixAnySequenceLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "SuffixArray", run_SuffixArray, [.validation, .api, .Array])
addTo(&precommitTests, "SuffixArrayLazy", run_SuffixArrayLazy, [.validation, .api, .Array])
addTo(&precommitTests, "SuffixCountableRange", run_SuffixCountableRange, [.validation, .api, .abstraction])
addTo(&precommitTests, "SuffixCountableRangeLazy", run_SuffixCountableRangeLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "SuffixSequence", run_SuffixSequence, [.validation, .api, .abstraction])
addTo(&precommitTests, "SuffixSequenceLazy", run_SuffixSequenceLazy, [.validation, .api, .abstraction])
addTo(&precommitTests, "SumUsingReduce", run_SumUsingReduce, [.validation, .api])
addTo(&precommitTests, "SumUsingReduceInto", run_SumUsingReduceInto, [.validation, .api])
addTo(&precommitTests, "SuperChars", run_SuperChars, [.validation, .api, .String])
addTo(&precommitTests, "TwoSum", run_TwoSum, [.validation, .api, .Dictionary, .Array, .algorithm])
addTo(&precommitTests, "TypeFlood", run_TypeFlood, [.validation, .metadata])
addTo(&precommitTests, "UTF8Decode", run_UTF8Decode, [.validation, .api, .String])
addTo(&precommitTests, "Walsh", run_Walsh, [.validation, .algorithm])
addTo(&precommitTests, "XorLoop", run_XorLoop, [.validation])
addTo(&precommitTests, "accessGlobal", run_accessGlobal, [.regression])
addTo(&precommitTests, "accessInMatSet", run_accessInMatSet, [.regression])
addTo(&precommitTests, "accessIndependent", run_accessIndependent, [.regression])

// Other tests
addTo(&otherTests, "Ackermann", run_Ackermann, [.unstable, .algorithm])
addTo(&otherTests, "ExistentialTestArrayConditionalShift_ClassValueBuffer1", run_ExistentialTestArrayConditionalShift_ClassValueBuffer1, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayConditionalShift_ClassValueBuffer2", run_ExistentialTestArrayConditionalShift_ClassValueBuffer2, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayConditionalShift_ClassValueBuffer3", run_ExistentialTestArrayConditionalShift_ClassValueBuffer3, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayConditionalShift_ClassValueBuffer4", run_ExistentialTestArrayConditionalShift_ClassValueBuffer4, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayConditionalShift_IntValueBuffer0", run_ExistentialTestArrayConditionalShift_IntValueBuffer0, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayConditionalShift_IntValueBuffer1", run_ExistentialTestArrayConditionalShift_IntValueBuffer1, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayConditionalShift_IntValueBuffer2", run_ExistentialTestArrayConditionalShift_IntValueBuffer2, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayConditionalShift_IntValueBuffer3", run_ExistentialTestArrayConditionalShift_IntValueBuffer3, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayConditionalShift_IntValueBuffer4", run_ExistentialTestArrayConditionalShift_IntValueBuffer4, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayMutating_ClassValueBuffer1", run_ExistentialTestArrayMutating_ClassValueBuffer1, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayMutating_ClassValueBuffer2", run_ExistentialTestArrayMutating_ClassValueBuffer2, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayMutating_ClassValueBuffer3", run_ExistentialTestArrayMutating_ClassValueBuffer3, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayMutating_ClassValueBuffer4", run_ExistentialTestArrayMutating_ClassValueBuffer4, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayMutating_IntValueBuffer0", run_ExistentialTestArrayMutating_IntValueBuffer0, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayMutating_IntValueBuffer1", run_ExistentialTestArrayMutating_IntValueBuffer1, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayMutating_IntValueBuffer2", run_ExistentialTestArrayMutating_IntValueBuffer2, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayMutating_IntValueBuffer3", run_ExistentialTestArrayMutating_IntValueBuffer3, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayMutating_IntValueBuffer4", run_ExistentialTestArrayMutating_IntValueBuffer4, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayOneMethodCall_ClassValueBuffer1", run_ExistentialTestArrayOneMethodCall_ClassValueBuffer1, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayOneMethodCall_ClassValueBuffer2", run_ExistentialTestArrayOneMethodCall_ClassValueBuffer2, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayOneMethodCall_ClassValueBuffer3", run_ExistentialTestArrayOneMethodCall_ClassValueBuffer3, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayOneMethodCall_ClassValueBuffer4", run_ExistentialTestArrayOneMethodCall_ClassValueBuffer4, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayOneMethodCall_IntValueBuffer0", run_ExistentialTestArrayOneMethodCall_IntValueBuffer0, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayOneMethodCall_IntValueBuffer1", run_ExistentialTestArrayOneMethodCall_IntValueBuffer1, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayOneMethodCall_IntValueBuffer2", run_ExistentialTestArrayOneMethodCall_IntValueBuffer2, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayOneMethodCall_IntValueBuffer3", run_ExistentialTestArrayOneMethodCall_IntValueBuffer3, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayOneMethodCall_IntValueBuffer4", run_ExistentialTestArrayOneMethodCall_IntValueBuffer4, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayShift_ClassValueBuffer1", run_ExistentialTestArrayShift_ClassValueBuffer1, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayShift_ClassValueBuffer2", run_ExistentialTestArrayShift_ClassValueBuffer2, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayShift_ClassValueBuffer3", run_ExistentialTestArrayShift_ClassValueBuffer3, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayShift_ClassValueBuffer4", run_ExistentialTestArrayShift_ClassValueBuffer4, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayShift_IntValueBuffer0", run_ExistentialTestArrayShift_IntValueBuffer0, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayShift_IntValueBuffer1", run_ExistentialTestArrayShift_IntValueBuffer1, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayShift_IntValueBuffer2", run_ExistentialTestArrayShift_IntValueBuffer2, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayShift_IntValueBuffer3", run_ExistentialTestArrayShift_IntValueBuffer3, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayShift_IntValueBuffer4", run_ExistentialTestArrayShift_IntValueBuffer4, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayTwoMethodCalls_ClassValueBuffer1", run_ExistentialTestArrayTwoMethodCalls_ClassValueBuffer1, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayTwoMethodCalls_ClassValueBuffer2", run_ExistentialTestArrayTwoMethodCalls_ClassValueBuffer2, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayTwoMethodCalls_ClassValueBuffer3", run_ExistentialTestArrayTwoMethodCalls_ClassValueBuffer3, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayTwoMethodCalls_ClassValueBuffer4", run_ExistentialTestArrayTwoMethodCalls_ClassValueBuffer4, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayTwoMethodCalls_IntValueBuffer0", run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer0, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayTwoMethodCalls_IntValueBuffer1", run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer1, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayTwoMethodCalls_IntValueBuffer2", run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer2, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayTwoMethodCalls_IntValueBuffer3", run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer3, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestArrayTwoMethodCalls_IntValueBuffer4", run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer4, [.unstable, .api, .Array])
addTo(&otherTests, "ExistentialTestMutatingAndNonMutating_ClassValueBuffer1", run_ExistentialTestMutatingAndNonMutating_ClassValueBuffer1, [.unstable])
addTo(&otherTests, "ExistentialTestMutatingAndNonMutating_ClassValueBuffer2", run_ExistentialTestMutatingAndNonMutating_ClassValueBuffer2, [.unstable])
addTo(&otherTests, "ExistentialTestMutatingAndNonMutating_ClassValueBuffer3", run_ExistentialTestMutatingAndNonMutating_ClassValueBuffer3, [.unstable])
addTo(&otherTests, "ExistentialTestMutatingAndNonMutating_ClassValueBuffer4", run_ExistentialTestMutatingAndNonMutating_ClassValueBuffer4, [.unstable])
addTo(&otherTests, "ExistentialTestMutatingAndNonMutating_IntValueBuffer0", run_ExistentialTestMutatingAndNonMutating_IntValueBuffer0, [.unstable])
addTo(&otherTests, "ExistentialTestMutatingAndNonMutating_IntValueBuffer1", run_ExistentialTestMutatingAndNonMutating_IntValueBuffer1, [.unstable])
addTo(&otherTests, "ExistentialTestMutatingAndNonMutating_IntValueBuffer2", run_ExistentialTestMutatingAndNonMutating_IntValueBuffer2, [.unstable])
addTo(&otherTests, "ExistentialTestMutatingAndNonMutating_IntValueBuffer3", run_ExistentialTestMutatingAndNonMutating_IntValueBuffer3, [.unstable])
addTo(&otherTests, "ExistentialTestMutatingAndNonMutating_IntValueBuffer4", run_ExistentialTestMutatingAndNonMutating_IntValueBuffer4, [.unstable])
addTo(&otherTests, "ExistentialTestMutating_ClassValueBuffer1", run_ExistentialTestMutating_ClassValueBuffer1, [.unstable])
addTo(&otherTests, "ExistentialTestMutating_ClassValueBuffer2", run_ExistentialTestMutating_ClassValueBuffer2, [.unstable])
addTo(&otherTests, "ExistentialTestMutating_ClassValueBuffer3", run_ExistentialTestMutating_ClassValueBuffer3, [.unstable])
addTo(&otherTests, "ExistentialTestMutating_ClassValueBuffer4", run_ExistentialTestMutating_ClassValueBuffer4, [.unstable])
addTo(&otherTests, "ExistentialTestMutating_IntValueBuffer0", run_ExistentialTestMutating_IntValueBuffer0, [.unstable])
addTo(&otherTests, "ExistentialTestMutating_IntValueBuffer1", run_ExistentialTestMutating_IntValueBuffer1, [.unstable])
addTo(&otherTests, "ExistentialTestMutating_IntValueBuffer2", run_ExistentialTestMutating_IntValueBuffer2, [.unstable])
addTo(&otherTests, "ExistentialTestMutating_IntValueBuffer3", run_ExistentialTestMutating_IntValueBuffer3, [.unstable])
addTo(&otherTests, "ExistentialTestMutating_IntValueBuffer4", run_ExistentialTestMutating_IntValueBuffer4, [.unstable])
addTo(&otherTests, "ExistentialTestOneMethodCall_ClassValueBuffer1", run_ExistentialTestOneMethodCall_ClassValueBuffer1, [.unstable])
addTo(&otherTests, "ExistentialTestOneMethodCall_ClassValueBuffer2", run_ExistentialTestOneMethodCall_ClassValueBuffer2, [.unstable])
addTo(&otherTests, "ExistentialTestOneMethodCall_ClassValueBuffer3", run_ExistentialTestOneMethodCall_ClassValueBuffer3, [.unstable])
addTo(&otherTests, "ExistentialTestOneMethodCall_ClassValueBuffer4", run_ExistentialTestOneMethodCall_ClassValueBuffer4, [.unstable])
addTo(&otherTests, "ExistentialTestOneMethodCall_IntValueBuffer0", run_ExistentialTestOneMethodCall_IntValueBuffer0, [.unstable])
addTo(&otherTests, "ExistentialTestOneMethodCall_IntValueBuffer1", run_ExistentialTestOneMethodCall_IntValueBuffer1, [.unstable])
addTo(&otherTests, "ExistentialTestOneMethodCall_IntValueBuffer2", run_ExistentialTestOneMethodCall_IntValueBuffer2, [.unstable])
addTo(&otherTests, "ExistentialTestOneMethodCall_IntValueBuffer3", run_ExistentialTestOneMethodCall_IntValueBuffer3, [.unstable])
addTo(&otherTests, "ExistentialTestOneMethodCall_IntValueBuffer4", run_ExistentialTestOneMethodCall_IntValueBuffer4, [.unstable])
addTo(&otherTests, "ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer1", run_ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer1, [.unstable])
addTo(&otherTests, "ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer2", run_ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer2, [.unstable])
addTo(&otherTests, "ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer3", run_ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer3, [.unstable])
addTo(&otherTests, "ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer4", run_ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer4, [.unstable])
addTo(&otherTests, "ExistentialTestPassExistentialOneMethodCall_IntValueBuffer0", run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer0, [.unstable])
addTo(&otherTests, "ExistentialTestPassExistentialOneMethodCall_IntValueBuffer1", run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer1, [.unstable])
addTo(&otherTests, "ExistentialTestPassExistentialOneMethodCall_IntValueBuffer2", run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer2, [.unstable])
addTo(&otherTests, "ExistentialTestPassExistentialOneMethodCall_IntValueBuffer3", run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer3, [.unstable])
addTo(&otherTests, "ExistentialTestPassExistentialOneMethodCall_IntValueBuffer4", run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer4, [.unstable])
addTo(&otherTests, "ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer1", run_ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer1, [.unstable])
addTo(&otherTests, "ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer2", run_ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer2, [.unstable])
addTo(&otherTests, "ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer3", run_ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer3, [.unstable])
addTo(&otherTests, "ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer4", run_ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer4, [.unstable])
addTo(&otherTests, "ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer0", run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer0, [.unstable])
addTo(&otherTests, "ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer1", run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer1, [.unstable])
addTo(&otherTests, "ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer2", run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer2, [.unstable])
addTo(&otherTests, "ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer3", run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer3, [.unstable])
addTo(&otherTests, "ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer4", run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer4, [.unstable])
addTo(&otherTests, "ExistentialTestTwoMethodCalls_ClassValueBuffer1", run_ExistentialTestTwoMethodCalls_ClassValueBuffer1, [.unstable])
addTo(&otherTests, "ExistentialTestTwoMethodCalls_ClassValueBuffer2", run_ExistentialTestTwoMethodCalls_ClassValueBuffer2, [.unstable])
addTo(&otherTests, "ExistentialTestTwoMethodCalls_ClassValueBuffer3", run_ExistentialTestTwoMethodCalls_ClassValueBuffer3, [.unstable])
addTo(&otherTests, "ExistentialTestTwoMethodCalls_ClassValueBuffer4", run_ExistentialTestTwoMethodCalls_ClassValueBuffer4, [.unstable])
addTo(&otherTests, "ExistentialTestTwoMethodCalls_IntValueBuffer0", run_ExistentialTestTwoMethodCalls_IntValueBuffer0, [.unstable])
addTo(&otherTests, "ExistentialTestTwoMethodCalls_IntValueBuffer1", run_ExistentialTestTwoMethodCalls_IntValueBuffer1, [.unstable])
addTo(&otherTests, "ExistentialTestTwoMethodCalls_IntValueBuffer2", run_ExistentialTestTwoMethodCalls_IntValueBuffer2, [.unstable])
addTo(&otherTests, "ExistentialTestTwoMethodCalls_IntValueBuffer3", run_ExistentialTestTwoMethodCalls_IntValueBuffer3, [.unstable])
addTo(&otherTests, "ExistentialTestTwoMethodCalls_IntValueBuffer4", run_ExistentialTestTwoMethodCalls_IntValueBuffer4, [.unstable])
addTo(&otherTests, "Fibonacci", run_Fibonacci, [.unstable, .algorithm])
addTo(&otherTests, "HashQuadratic", run_HashQuadratic, [.unstable, .api, .Dictionary])

// String tests, an extended benchmark suite exercising finer-granularity
// behavior of our Strings.
addTo(&stringTests, "StringWalk_ascii_characters", run_StringWalk_ascii_characters, [.api, .String])
addTo(&stringTests, "StringWalk_ascii_characters_Backwards", run_StringWalk_ascii_characters_Backwards, [.api, .String])
addTo(&stringTests, "StringWalk_ascii_unicodeScalars", run_StringWalk_ascii_unicodeScalars, [.api, .String])
addTo(&stringTests, "StringWalk_ascii_unicodeScalars_Backwards", run_StringWalk_ascii_unicodeScalars_Backwards, [.api, .String])
addTo(&stringTests, "StringWalk_chinese_characters", run_StringWalk_chinese_characters, [.api, .String])
addTo(&stringTests, "StringWalk_chinese_characters_Backwards", run_StringWalk_chinese_characters_Backwards, [.api, .String])
addTo(&stringTests, "StringWalk_chinese_unicodeScalars", run_StringWalk_chinese_unicodeScalars, [.api, .String])
addTo(&stringTests, "StringWalk_chinese_unicodeScalars_Backwards", run_StringWalk_chinese_unicodeScalars_Backwards, [.api, .String])
addTo(&stringTests, "StringWalk_japanese_characters", run_StringWalk_japanese_characters, [.api, .String])
addTo(&stringTests, "StringWalk_japanese_characters_Backwards", run_StringWalk_japanese_characters_Backwards, [.api, .String])
addTo(&stringTests, "StringWalk_japanese_unicodeScalars", run_StringWalk_japanese_unicodeScalars, [.api, .String])
addTo(&stringTests, "StringWalk_japanese_unicodeScalars_Backwards", run_StringWalk_japanese_unicodeScalars_Backwards, [.api, .String])
addTo(&stringTests, "StringWalk_korean_characters", run_StringWalk_korean_characters, [.api, .String])
addTo(&stringTests, "StringWalk_korean_characters_Backwards", run_StringWalk_korean_characters_Backwards, [.api, .String])
addTo(&stringTests, "StringWalk_korean_unicodeScalars", run_StringWalk_korean_unicodeScalars, [.api, .String])
addTo(&stringTests, "StringWalk_korean_unicodeScalars_Backwards", run_StringWalk_korean_unicodeScalars_Backwards, [.api, .String])
addTo(&stringTests, "StringWalk_punctuatedJapanese_characters", run_StringWalk_punctuatedJapanese_characters, [.api, .String])
addTo(&stringTests, "StringWalk_punctuatedJapanese_characters_Backwards", run_StringWalk_punctuatedJapanese_characters_Backwards, [.api, .String])
addTo(&stringTests, "StringWalk_punctuatedJapanese_unicodeScalars", run_StringWalk_punctuatedJapanese_unicodeScalars, [.api, .String])
addTo(&stringTests, "StringWalk_punctuatedJapanese_unicodeScalars_Backwards", run_StringWalk_punctuatedJapanese_unicodeScalars_Backwards, [.api, .String])
addTo(&stringTests, "StringWalk_punctuated_characters", run_StringWalk_punctuated_characters, [.api, .String])
addTo(&stringTests, "StringWalk_punctuated_characters_Backwards", run_StringWalk_punctuated_characters_Backwards, [.api, .String])
addTo(&stringTests, "StringWalk_punctuated_unicodeScalars", run_StringWalk_punctuated_unicodeScalars, [.api, .String])
addTo(&stringTests, "StringWalk_punctuated_unicodeScalars_Backwards", run_StringWalk_punctuated_unicodeScalars_Backwards, [.api, .String])
addTo(&stringTests, "StringWalk_russian_characters", run_StringWalk_russian_characters, [.api, .String])
addTo(&stringTests, "StringWalk_russian_characters_Backwards", run_StringWalk_russian_characters_Backwards, [.api, .String])
addTo(&stringTests, "StringWalk_russian_unicodeScalars", run_StringWalk_russian_unicodeScalars, [.api, .String])
addTo(&stringTests, "StringWalk_russian_unicodeScalars_Backwards", run_StringWalk_russian_unicodeScalars_Backwards, [.api, .String])
addTo(&stringTests, "StringWalk_tweet_characters", run_StringWalk_tweet_characters, [.api, .String])
addTo(&stringTests, "StringWalk_tweet_characters_Backwards", run_StringWalk_tweet_characters_Backwards, [.api, .String])
addTo(&stringTests, "StringWalk_tweet_unicodeScalars", run_StringWalk_tweet_unicodeScalars, [.api, .String])
addTo(&stringTests, "StringWalk_tweet_unicodeScalars_Backwards", run_StringWalk_tweet_unicodeScalars_Backwards, [.api, .String])
addTo(&stringTests, "StringWalk_utf16_characters", run_StringWalk_utf16_characters, [.api, .String])
addTo(&stringTests, "StringWalk_utf16_characters_Backwards", run_StringWalk_utf16_characters_Backwards, [.api, .String])
addTo(&stringTests, "StringWalk_utf16_unicodeScalars", run_StringWalk_utf16_unicodeScalars, [.api, .String])
addTo(&stringTests, "StringWalk_utf16_unicodeScalars_Backwards", run_StringWalk_utf16_unicodeScalars_Backwards, [.api, .String])

main()
