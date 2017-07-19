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
import ProtocolDispatch
import ProtocolDispatch2
import RC4
import RGBHistogram
import RangeAssignment
import RecursiveOwnedParameter
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
private func addTo(
  _ testSuite: inout [String : (Int) -> ()],
  _ name: String,
  _ function: @escaping (Int) -> ()
) {
  testSuite[name] = function
}

// The main test suite: precommit tests
addTo(&precommitTests, "AngryPhonebook", run_AngryPhonebook)
addTo(&precommitTests, "AnyHashableWithAClass", run_AnyHashableWithAClass)
addTo(&precommitTests, "Array2D", run_Array2D)
addTo(&precommitTests, "ArrayAppend", run_ArrayAppend)
addTo(&precommitTests, "ArrayAppendArrayOfInt", run_ArrayAppendArrayOfInt)
addTo(&precommitTests, "ArrayAppendAscii", run_ArrayAppendAscii)
addTo(&precommitTests, "ArrayAppendFromGeneric", run_ArrayAppendFromGeneric)
addTo(&precommitTests, "ArrayAppendGenericStructs", run_ArrayAppendGenericStructs)
addTo(&precommitTests, "ArrayAppendLatin1", run_ArrayAppendLatin1)
addTo(&precommitTests, "ArrayAppendLazyMap", run_ArrayAppendLazyMap)
addTo(&precommitTests, "ArrayAppendOptionals", run_ArrayAppendOptionals)
addTo(&precommitTests, "ArrayAppendRepeatCol", run_ArrayAppendRepeatCol)
addTo(&precommitTests, "ArrayAppendReserved", run_ArrayAppendReserved)
addTo(&precommitTests, "ArrayAppendSequence", run_ArrayAppendSequence)
addTo(&precommitTests, "ArrayAppendStrings", run_ArrayAppendStrings)
addTo(&precommitTests, "ArrayAppendToFromGeneric", run_ArrayAppendToFromGeneric)
addTo(&precommitTests, "ArrayAppendToGeneric", run_ArrayAppendToGeneric)
addTo(&precommitTests, "ArrayAppendUTF16", run_ArrayAppendUTF16)
addTo(&precommitTests, "ArrayInClass", run_ArrayInClass)
addTo(&precommitTests, "ArrayLiteral", run_ArrayLiteral)
addTo(&precommitTests, "ArrayOfGenericPOD", run_ArrayOfGenericPOD)
addTo(&precommitTests, "ArrayOfGenericRef", run_ArrayOfGenericRef)
addTo(&precommitTests, "ArrayOfPOD", run_ArrayOfPOD)
addTo(&precommitTests, "ArrayOfRef", run_ArrayOfRef)
addTo(&precommitTests, "ArrayPlusEqualArrayOfInt", run_ArrayPlusEqualArrayOfInt)
addTo(&precommitTests, "ArrayPlusEqualFiveElementCollection", run_ArrayPlusEqualFiveElementCollection)
addTo(&precommitTests, "ArrayPlusEqualSingleElementCollection", run_ArrayPlusEqualSingleElementCollection)
addTo(&precommitTests, "ArrayPlusEqualThreeElements", run_ArrayPlusEqualThreeElements)
addTo(&precommitTests, "ArraySubscript", run_ArraySubscript)
addTo(&precommitTests, "ArrayValueProp", run_ArrayValueProp)
addTo(&precommitTests, "ArrayValueProp2", run_ArrayValueProp2)
addTo(&precommitTests, "ArrayValueProp3", run_ArrayValueProp3)
addTo(&precommitTests, "ArrayValueProp4", run_ArrayValueProp4)
addTo(&precommitTests, "BitCount", run_BitCount)
addTo(&precommitTests, "ByteSwap", run_ByteSwap)
addTo(&precommitTests, "CStringLongAscii", run_CStringLongAscii)
addTo(&precommitTests, "CStringLongNonAscii", run_CStringLongNonAscii)
addTo(&precommitTests, "CStringShortAscii", run_CStringShortAscii)
addTo(&precommitTests, "Calculator", run_Calculator)
addTo(&precommitTests, "CaptureProp", run_CaptureProp)
addTo(&precommitTests, "CharIndexing_ascii_unicodeScalars", run_CharIndexing_ascii_unicodeScalars)
addTo(&precommitTests, "CharIndexing_ascii_unicodeScalars_Backwards", run_CharIndexing_ascii_unicodeScalars_Backwards)
addTo(&precommitTests, "CharIndexing_chinese_unicodeScalars", run_CharIndexing_chinese_unicodeScalars)
addTo(&precommitTests, "CharIndexing_chinese_unicodeScalars_Backwards", run_CharIndexing_chinese_unicodeScalars_Backwards)
addTo(&precommitTests, "CharIndexing_japanese_unicodeScalars", run_CharIndexing_japanese_unicodeScalars)
addTo(&precommitTests, "CharIndexing_japanese_unicodeScalars_Backwards", run_CharIndexing_japanese_unicodeScalars_Backwards)
addTo(&precommitTests, "CharIndexing_korean_unicodeScalars", run_CharIndexing_korean_unicodeScalars)
addTo(&precommitTests, "CharIndexing_korean_unicodeScalars_Backwards", run_CharIndexing_korean_unicodeScalars_Backwards)
addTo(&precommitTests, "CharIndexing_punctuatedJapanese_unicodeScalars", run_CharIndexing_punctuatedJapanese_unicodeScalars)
addTo(&precommitTests, "CharIndexing_punctuatedJapanese_unicodeScalars_Backwards", run_CharIndexing_punctuatedJapanese_unicodeScalars_Backwards)
addTo(&precommitTests, "CharIndexing_punctuated_unicodeScalars", run_CharIndexing_punctuated_unicodeScalars)
addTo(&precommitTests, "CharIndexing_punctuated_unicodeScalars_Backwards", run_CharIndexing_punctuated_unicodeScalars_Backwards)
addTo(&precommitTests, "CharIndexing_russian_unicodeScalars", run_CharIndexing_russian_unicodeScalars)
addTo(&precommitTests, "CharIndexing_russian_unicodeScalars_Backwards", run_CharIndexing_russian_unicodeScalars_Backwards)
addTo(&precommitTests, "CharIndexing_tweet_unicodeScalars", run_CharIndexing_tweet_unicodeScalars)
addTo(&precommitTests, "CharIndexing_tweet_unicodeScalars_Backwards", run_CharIndexing_tweet_unicodeScalars_Backwards)
addTo(&precommitTests, "CharIndexing_utf16_unicodeScalars", run_CharIndexing_utf16_unicodeScalars)
addTo(&precommitTests, "CharIndexing_utf16_unicodeScalars_Backwards", run_CharIndexing_utf16_unicodeScalars_Backwards)
addTo(&precommitTests, "CharIteration_ascii_unicodeScalars", run_CharIteration_ascii_unicodeScalars)
addTo(&precommitTests, "CharIteration_ascii_unicodeScalars_Backwards", run_CharIteration_ascii_unicodeScalars_Backwards)
addTo(&precommitTests, "CharIteration_chinese_unicodeScalars", run_CharIteration_chinese_unicodeScalars)
addTo(&precommitTests, "CharIteration_chinese_unicodeScalars_Backwards", run_CharIteration_chinese_unicodeScalars_Backwards)
addTo(&precommitTests, "CharIteration_japanese_unicodeScalars", run_CharIteration_japanese_unicodeScalars)
addTo(&precommitTests, "CharIteration_japanese_unicodeScalars_Backwards", run_CharIteration_japanese_unicodeScalars_Backwards)
addTo(&precommitTests, "CharIteration_korean_unicodeScalars", run_CharIteration_korean_unicodeScalars)
addTo(&precommitTests, "CharIteration_korean_unicodeScalars_Backwards", run_CharIteration_korean_unicodeScalars_Backwards)
addTo(&precommitTests, "CharIteration_punctuatedJapanese_unicodeScalars", run_CharIteration_punctuatedJapanese_unicodeScalars)
addTo(&precommitTests, "CharIteration_punctuatedJapanese_unicodeScalars_Backwards", run_CharIteration_punctuatedJapanese_unicodeScalars_Backwards)
addTo(&precommitTests, "CharIteration_punctuated_unicodeScalars", run_CharIteration_punctuated_unicodeScalars)
addTo(&precommitTests, "CharIteration_punctuated_unicodeScalars_Backwards", run_CharIteration_punctuated_unicodeScalars_Backwards)
addTo(&precommitTests, "CharIteration_russian_unicodeScalars", run_CharIteration_russian_unicodeScalars)
addTo(&precommitTests, "CharIteration_russian_unicodeScalars_Backwards", run_CharIteration_russian_unicodeScalars_Backwards)
addTo(&precommitTests, "CharIteration_tweet_unicodeScalars", run_CharIteration_tweet_unicodeScalars)
addTo(&precommitTests, "CharIteration_tweet_unicodeScalars_Backwards", run_CharIteration_tweet_unicodeScalars_Backwards)
addTo(&precommitTests, "CharIteration_utf16_unicodeScalars", run_CharIteration_utf16_unicodeScalars)
addTo(&precommitTests, "CharIteration_utf16_unicodeScalars_Backwards", run_CharIteration_utf16_unicodeScalars_Backwards)
addTo(&precommitTests, "CharacterLiteralsLarge", run_CharacterLiteralsLarge)
addTo(&precommitTests, "CharacterLiteralsSmall", run_CharacterLiteralsSmall)
addTo(&precommitTests, "Chars", run_Chars)
addTo(&precommitTests, "ClassArrayGetter", run_ClassArrayGetter)
addTo(&precommitTests, "DeadArray", run_DeadArray)
addTo(&precommitTests, "Dictionary", run_Dictionary)
addTo(&precommitTests, "Dictionary2", run_Dictionary2)
addTo(&precommitTests, "Dictionary2OfObjects", run_Dictionary2OfObjects)
addTo(&precommitTests, "Dictionary3", run_Dictionary3)
addTo(&precommitTests, "Dictionary3OfObjects", run_Dictionary3OfObjects)
addTo(&precommitTests, "DictionaryBridge", run_DictionaryBridge)
addTo(&precommitTests, "DictionaryGroup", run_DictionaryGroup)
addTo(&precommitTests, "DictionaryGroupOfObjects", run_DictionaryGroupOfObjects)
addTo(&precommitTests, "DictionaryLiteral", run_DictionaryLiteral)
addTo(&precommitTests, "DictionaryOfObjects", run_DictionaryOfObjects)
addTo(&precommitTests, "DictionaryRemove", run_DictionaryRemove)
addTo(&precommitTests, "DictionaryRemoveOfObjects", run_DictionaryRemoveOfObjects)
addTo(&precommitTests, "DictionarySwap", run_DictionarySwap)
addTo(&precommitTests, "DictionarySwapOfObjects", run_DictionarySwapOfObjects)
addTo(&precommitTests, "DropFirstAnyCollection", run_DropFirstAnyCollection)
addTo(&precommitTests, "DropFirstAnyCollectionLazy", run_DropFirstAnyCollectionLazy)
addTo(&precommitTests, "DropFirstAnySeqCRangeIter", run_DropFirstAnySeqCRangeIter)
addTo(&precommitTests, "DropFirstAnySeqCRangeIterLazy", run_DropFirstAnySeqCRangeIterLazy)
addTo(&precommitTests, "DropFirstAnySeqCntRange", run_DropFirstAnySeqCntRange)
addTo(&precommitTests, "DropFirstAnySeqCntRangeLazy", run_DropFirstAnySeqCntRangeLazy)
addTo(&precommitTests, "DropFirstAnySequence", run_DropFirstAnySequence)
addTo(&precommitTests, "DropFirstAnySequenceLazy", run_DropFirstAnySequenceLazy)
addTo(&precommitTests, "DropFirstArray", run_DropFirstArray)
addTo(&precommitTests, "DropFirstArrayLazy", run_DropFirstArrayLazy)
addTo(&precommitTests, "DropFirstCountableRange", run_DropFirstCountableRange)
addTo(&precommitTests, "DropFirstCountableRangeLazy", run_DropFirstCountableRangeLazy)
addTo(&precommitTests, "DropFirstSequence", run_DropFirstSequence)
addTo(&precommitTests, "DropFirstSequenceLazy", run_DropFirstSequenceLazy)
addTo(&precommitTests, "DropLastAnyCollection", run_DropLastAnyCollection)
addTo(&precommitTests, "DropLastAnyCollectionLazy", run_DropLastAnyCollectionLazy)
addTo(&precommitTests, "DropLastAnySeqCRangeIter", run_DropLastAnySeqCRangeIter)
addTo(&precommitTests, "DropLastAnySeqCRangeIterLazy", run_DropLastAnySeqCRangeIterLazy)
addTo(&precommitTests, "DropLastAnySeqCntRange", run_DropLastAnySeqCntRange)
addTo(&precommitTests, "DropLastAnySeqCntRangeLazy", run_DropLastAnySeqCntRangeLazy)
addTo(&precommitTests, "DropLastAnySequence", run_DropLastAnySequence)
addTo(&precommitTests, "DropLastAnySequenceLazy", run_DropLastAnySequenceLazy)
addTo(&precommitTests, "DropLastArray", run_DropLastArray)
addTo(&precommitTests, "DropLastArrayLazy", run_DropLastArrayLazy)
addTo(&precommitTests, "DropLastCountableRange", run_DropLastCountableRange)
addTo(&precommitTests, "DropLastCountableRangeLazy", run_DropLastCountableRangeLazy)
addTo(&precommitTests, "DropLastSequence", run_DropLastSequence)
addTo(&precommitTests, "DropLastSequenceLazy", run_DropLastSequenceLazy)
addTo(&precommitTests, "DropWhileAnyCollection", run_DropWhileAnyCollection)
addTo(&precommitTests, "DropWhileAnyCollectionLazy", run_DropWhileAnyCollectionLazy)
addTo(&precommitTests, "DropWhileAnySeqCRangeIter", run_DropWhileAnySeqCRangeIter)
addTo(&precommitTests, "DropWhileAnySeqCRangeIterLazy", run_DropWhileAnySeqCRangeIterLazy)
addTo(&precommitTests, "DropWhileAnySeqCntRange", run_DropWhileAnySeqCntRange)
addTo(&precommitTests, "DropWhileAnySeqCntRangeLazy", run_DropWhileAnySeqCntRangeLazy)
addTo(&precommitTests, "DropWhileAnySequence", run_DropWhileAnySequence)
addTo(&precommitTests, "DropWhileAnySequenceLazy", run_DropWhileAnySequenceLazy)
addTo(&precommitTests, "DropWhileArray", run_DropWhileArray)
addTo(&precommitTests, "DropWhileArrayLazy", run_DropWhileArrayLazy)
addTo(&precommitTests, "DropWhileCountableRange", run_DropWhileCountableRange)
addTo(&precommitTests, "DropWhileCountableRangeLazy", run_DropWhileCountableRangeLazy)
addTo(&precommitTests, "DropWhileSequence", run_DropWhileSequence)
addTo(&precommitTests, "DropWhileSequenceLazy", run_DropWhileSequenceLazy)
addTo(&precommitTests, "EqualStringSubstring", run_EqualStringSubstring)
addTo(&precommitTests, "EqualSubstringString", run_EqualSubstringString)
addTo(&precommitTests, "EqualSubstringSubstring", run_EqualSubstringSubstring)
addTo(&precommitTests, "EqualSubstringSubstringGenericEquatable", run_EqualSubstringSubstringGenericEquatable)
addTo(&precommitTests, "ErrorHandling", run_ErrorHandling)
addTo(&precommitTests, "Hanoi", run_Hanoi)
addTo(&precommitTests, "HashTest", run_HashTest)
addTo(&precommitTests, "Histogram", run_Histogram)
addTo(&precommitTests, "Integrate", run_Integrate)
addTo(&precommitTests, "IterateData", run_IterateData)
addTo(&precommitTests, "Join", run_Join)
addTo(&precommitTests, "LazilyFilteredArrays", run_LazilyFilteredArrays)
addTo(&precommitTests, "LazilyFilteredRange", run_LazilyFilteredRange)
addTo(&precommitTests, "LessSubstringSubstring", run_LessSubstringSubstring)
addTo(&precommitTests, "LessSubstringSubstringGenericComparable", run_LessSubstringSubstringGenericComparable)
addTo(&precommitTests, "LinkedList", run_LinkedList)
addTo(&precommitTests, "MapReduce", run_MapReduce)
addTo(&precommitTests, "MapReduceAnyCollection", run_MapReduceAnyCollection)
addTo(&precommitTests, "MapReduceAnyCollectionShort", run_MapReduceAnyCollectionShort)
addTo(&precommitTests, "MapReduceClass", run_MapReduceClass)
addTo(&precommitTests, "MapReduceClassShort", run_MapReduceClassShort)
addTo(&precommitTests, "MapReduceLazyCollection", run_MapReduceLazyCollection)
addTo(&precommitTests, "MapReduceLazyCollectionShort", run_MapReduceLazyCollectionShort)
addTo(&precommitTests, "MapReduceLazySequence", run_MapReduceLazySequence)
addTo(&precommitTests, "MapReduceSequence", run_MapReduceSequence)
addTo(&precommitTests, "MapReduceShort", run_MapReduceShort)
addTo(&precommitTests, "MapReduceShortString", run_MapReduceShortString)
addTo(&precommitTests, "MapReduceString", run_MapReduceString)
addTo(&precommitTests, "Memset", run_Memset)
addTo(&precommitTests, "MonteCarloE", run_MonteCarloE)
addTo(&precommitTests, "MonteCarloPi", run_MonteCarloPi)
addTo(&precommitTests, "NSDictionaryCastToSwift", run_NSDictionaryCastToSwift)
addTo(&precommitTests, "NSError", run_NSError)
addTo(&precommitTests, "NSStringConversion", run_NSStringConversion)
addTo(&precommitTests, "NopDeinit", run_NopDeinit)
addTo(&precommitTests, "ObjectAllocation", run_ObjectAllocation)
addTo(&precommitTests, "ObjectiveCBridgeFromNSArrayAnyObject", run_ObjectiveCBridgeFromNSArrayAnyObject)
addTo(&precommitTests, "ObjectiveCBridgeFromNSArrayAnyObjectForced", run_ObjectiveCBridgeFromNSArrayAnyObjectForced)
addTo(&precommitTests, "ObjectiveCBridgeFromNSArrayAnyObjectToString", run_ObjectiveCBridgeFromNSArrayAnyObjectToString)
addTo(&precommitTests, "ObjectiveCBridgeFromNSArrayAnyObjectToStringForced", run_ObjectiveCBridgeFromNSArrayAnyObjectToStringForced)
addTo(&precommitTests, "ObjectiveCBridgeFromNSDictionaryAnyObject", run_ObjectiveCBridgeFromNSDictionaryAnyObject)
addTo(&precommitTests, "ObjectiveCBridgeFromNSDictionaryAnyObjectForced", run_ObjectiveCBridgeFromNSDictionaryAnyObjectForced)
addTo(&precommitTests, "ObjectiveCBridgeFromNSDictionaryAnyObjectToString", run_ObjectiveCBridgeFromNSDictionaryAnyObjectToString)
addTo(&precommitTests, "ObjectiveCBridgeFromNSDictionaryAnyObjectToStringForced", run_ObjectiveCBridgeFromNSDictionaryAnyObjectToStringForced)
addTo(&precommitTests, "ObjectiveCBridgeFromNSSetAnyObject", run_ObjectiveCBridgeFromNSSetAnyObject)
addTo(&precommitTests, "ObjectiveCBridgeFromNSSetAnyObjectForced", run_ObjectiveCBridgeFromNSSetAnyObjectForced)
addTo(&precommitTests, "ObjectiveCBridgeFromNSSetAnyObjectToString", run_ObjectiveCBridgeFromNSSetAnyObjectToString)
addTo(&precommitTests, "ObjectiveCBridgeFromNSSetAnyObjectToStringForced", run_ObjectiveCBridgeFromNSSetAnyObjectToStringForced)
addTo(&precommitTests, "ObjectiveCBridgeFromNSString", run_ObjectiveCBridgeFromNSString)
addTo(&precommitTests, "ObjectiveCBridgeFromNSStringForced", run_ObjectiveCBridgeFromNSStringForced)
addTo(&precommitTests, "ObjectiveCBridgeStubDataAppend", run_ObjectiveCBridgeStubDataAppend)
addTo(&precommitTests, "ObjectiveCBridgeStubDateAccess", run_ObjectiveCBridgeStubDateAccess)
addTo(&precommitTests, "ObjectiveCBridgeStubDateMutation", run_ObjectiveCBridgeStubDateMutation)
addTo(&precommitTests, "ObjectiveCBridgeStubFromArrayOfNSString", run_ObjectiveCBridgeStubFromArrayOfNSString)
addTo(&precommitTests, "ObjectiveCBridgeStubFromNSDate", run_ObjectiveCBridgeStubFromNSDate)
addTo(&precommitTests, "ObjectiveCBridgeStubFromNSDateRef", run_ObjectiveCBridgeStubFromNSDateRef)
addTo(&precommitTests, "ObjectiveCBridgeStubFromNSString", run_ObjectiveCBridgeStubFromNSString)
addTo(&precommitTests, "ObjectiveCBridgeStubFromNSStringRef", run_ObjectiveCBridgeStubFromNSStringRef)
addTo(&precommitTests, "ObjectiveCBridgeStubNSDataAppend", run_ObjectiveCBridgeStubNSDataAppend)
addTo(&precommitTests, "ObjectiveCBridgeStubNSDateMutationRef", run_ObjectiveCBridgeStubNSDateMutationRef)
addTo(&precommitTests, "ObjectiveCBridgeStubNSDateRefAccess", run_ObjectiveCBridgeStubNSDateRefAccess)
addTo(&precommitTests, "ObjectiveCBridgeStubToArrayOfNSString", run_ObjectiveCBridgeStubToArrayOfNSString)
addTo(&precommitTests, "ObjectiveCBridgeStubToNSDate", run_ObjectiveCBridgeStubToNSDate)
addTo(&precommitTests, "ObjectiveCBridgeStubToNSDateRef", run_ObjectiveCBridgeStubToNSDateRef)
addTo(&precommitTests, "ObjectiveCBridgeStubToNSString", run_ObjectiveCBridgeStubToNSString)
addTo(&precommitTests, "ObjectiveCBridgeStubToNSStringRef", run_ObjectiveCBridgeStubToNSStringRef)
addTo(&precommitTests, "ObjectiveCBridgeStubURLAppendPath", run_ObjectiveCBridgeStubURLAppendPath)
addTo(&precommitTests, "ObjectiveCBridgeStubURLAppendPathRef", run_ObjectiveCBridgeStubURLAppendPathRef)
addTo(&precommitTests, "ObjectiveCBridgeToNSArray", run_ObjectiveCBridgeToNSArray)
addTo(&precommitTests, "ObjectiveCBridgeToNSDictionary", run_ObjectiveCBridgeToNSDictionary)
addTo(&precommitTests, "ObjectiveCBridgeToNSSet", run_ObjectiveCBridgeToNSSet)
addTo(&precommitTests, "ObjectiveCBridgeToNSString", run_ObjectiveCBridgeToNSString)
addTo(&precommitTests, "ObserverClosure", run_ObserverClosure)
addTo(&precommitTests, "ObserverForwarderStruct", run_ObserverForwarderStruct)
addTo(&precommitTests, "ObserverPartiallyAppliedMethod", run_ObserverPartiallyAppliedMethod)
addTo(&precommitTests, "ObserverUnappliedMethod", run_ObserverUnappliedMethod)
addTo(&precommitTests, "OpenClose", run_OpenClose)
addTo(&precommitTests, "Phonebook", run_Phonebook)
addTo(&precommitTests, "PolymorphicCalls", run_PolymorphicCalls)
addTo(&precommitTests, "PopFrontArray", run_PopFrontArray)
addTo(&precommitTests, "PopFrontArrayGeneric", run_PopFrontArrayGeneric)
addTo(&precommitTests, "PopFrontUnsafePointer", run_PopFrontUnsafePointer)
addTo(&precommitTests, "PrefixAnyCollection", run_PrefixAnyCollection)
addTo(&precommitTests, "PrefixAnyCollectionLazy", run_PrefixAnyCollectionLazy)
addTo(&precommitTests, "PrefixAnySeqCRangeIter", run_PrefixAnySeqCRangeIter)
addTo(&precommitTests, "PrefixAnySeqCRangeIterLazy", run_PrefixAnySeqCRangeIterLazy)
addTo(&precommitTests, "PrefixAnySeqCntRange", run_PrefixAnySeqCntRange)
addTo(&precommitTests, "PrefixAnySeqCntRangeLazy", run_PrefixAnySeqCntRangeLazy)
addTo(&precommitTests, "PrefixAnySequence", run_PrefixAnySequence)
addTo(&precommitTests, "PrefixAnySequenceLazy", run_PrefixAnySequenceLazy)
addTo(&precommitTests, "PrefixArray", run_PrefixArray)
addTo(&precommitTests, "PrefixArrayLazy", run_PrefixArrayLazy)
addTo(&precommitTests, "PrefixCountableRange", run_PrefixCountableRange)
addTo(&precommitTests, "PrefixCountableRangeLazy", run_PrefixCountableRangeLazy)
addTo(&precommitTests, "PrefixSequence", run_PrefixSequence)
addTo(&precommitTests, "PrefixSequenceLazy", run_PrefixSequenceLazy)
addTo(&precommitTests, "PrefixWhileAnyCollection", run_PrefixWhileAnyCollection)
addTo(&precommitTests, "PrefixWhileAnyCollectionLazy", run_PrefixWhileAnyCollectionLazy)
addTo(&precommitTests, "PrefixWhileAnySeqCRangeIter", run_PrefixWhileAnySeqCRangeIter)
addTo(&precommitTests, "PrefixWhileAnySeqCRangeIterLazy", run_PrefixWhileAnySeqCRangeIterLazy)
addTo(&precommitTests, "PrefixWhileAnySeqCntRange", run_PrefixWhileAnySeqCntRange)
addTo(&precommitTests, "PrefixWhileAnySeqCntRangeLazy", run_PrefixWhileAnySeqCntRangeLazy)
addTo(&precommitTests, "PrefixWhileAnySequence", run_PrefixWhileAnySequence)
addTo(&precommitTests, "PrefixWhileAnySequenceLazy", run_PrefixWhileAnySequenceLazy)
addTo(&precommitTests, "PrefixWhileArray", run_PrefixWhileArray)
addTo(&precommitTests, "PrefixWhileArrayLazy", run_PrefixWhileArrayLazy)
addTo(&precommitTests, "PrefixWhileCountableRange", run_PrefixWhileCountableRange)
addTo(&precommitTests, "PrefixWhileCountableRangeLazy", run_PrefixWhileCountableRangeLazy)
addTo(&precommitTests, "PrefixWhileSequence", run_PrefixWhileSequence)
addTo(&precommitTests, "PrefixWhileSequenceLazy", run_PrefixWhileSequenceLazy)
addTo(&precommitTests, "Prims", run_Prims)
addTo(&precommitTests, "ProtocolDispatch", run_ProtocolDispatch)
addTo(&precommitTests, "ProtocolDispatch2", run_ProtocolDispatch2)
addTo(&precommitTests, "RC4", run_RC4)
addTo(&precommitTests, "RGBHistogram", run_RGBHistogram)
addTo(&precommitTests, "RGBHistogramOfObjects", run_RGBHistogramOfObjects)
addTo(&precommitTests, "RangeAssignment", run_RangeAssignment)
addTo(&precommitTests, "RecursiveOwnedParameter", run_RecursiveOwnedParameter)
addTo(&precommitTests, "ReversedArray", run_ReversedArray)
addTo(&precommitTests, "ReversedBidirectional", run_ReversedBidirectional)
addTo(&precommitTests, "ReversedDictionary", run_ReversedDictionary)
addTo(&precommitTests, "SetExclusiveOr", run_SetExclusiveOr)
addTo(&precommitTests, "SetExclusiveOr_OfObjects", run_SetExclusiveOr_OfObjects)
addTo(&precommitTests, "SetIntersect", run_SetIntersect)
addTo(&precommitTests, "SetIntersect_OfObjects", run_SetIntersect_OfObjects)
addTo(&precommitTests, "SetIsSubsetOf", run_SetIsSubsetOf)
addTo(&precommitTests, "SetIsSubsetOf_OfObjects", run_SetIsSubsetOf_OfObjects)
addTo(&precommitTests, "SetUnion", run_SetUnion)
addTo(&precommitTests, "SetUnion_OfObjects", run_SetUnion_OfObjects)
addTo(&precommitTests, "SevenBoom", run_SevenBoom)
addTo(&precommitTests, "Sim2DArray", run_Sim2DArray)
addTo(&precommitTests, "SortLargeExistentials", run_SortLargeExistentials)
addTo(&precommitTests, "SortLettersInPlace", run_SortLettersInPlace)
addTo(&precommitTests, "SortSortedStrings", run_SortSortedStrings)
addTo(&precommitTests, "SortStrings", run_SortStrings)
addTo(&precommitTests, "SortStringsUnicode", run_SortStringsUnicode)
addTo(&precommitTests, "StackPromo", run_StackPromo)
addTo(&precommitTests, "StaticArray", run_StaticArray)
addTo(&precommitTests, "StrComplexWalk", run_StrComplexWalk)
addTo(&precommitTests, "StrToInt", run_StrToInt)
addTo(&precommitTests, "StringAdder", run_StringAdder)
addTo(&precommitTests, "StringBuilder", run_StringBuilder)
addTo(&precommitTests, "StringBuilderLong", run_StringBuilderLong)
addTo(&precommitTests, "StringEdits", run_StringEdits)
addTo(&precommitTests, "StringEqualPointerComparison", run_StringEqualPointerComparison)
addTo(&precommitTests, "StringFromLongWholeSubstring", run_StringFromLongWholeSubstring)
addTo(&precommitTests, "StringFromLongWholeSubstringGeneric", run_StringFromLongWholeSubstringGeneric)
addTo(&precommitTests, "StringHasPrefix", run_StringHasPrefix)
addTo(&precommitTests, "StringHasPrefixUnicode", run_StringHasPrefixUnicode)
addTo(&precommitTests, "StringHasSuffix", run_StringHasSuffix)
addTo(&precommitTests, "StringHasSuffixUnicode", run_StringHasSuffixUnicode)
addTo(&precommitTests, "StringInterpolation", run_StringInterpolation)
addTo(&precommitTests, "StringMatch", run_StringMatch)
addTo(&precommitTests, "StringUTF16Builder", run_StringUTF16Builder)
addTo(&precommitTests, "StringWalk", run_StringWalk)
addTo(&precommitTests, "StringWithCString", run_StringWithCString)
addTo(&precommitTests, "SubstringComparable", run_SubstringComparable)
addTo(&precommitTests, "SubstringEqualString", run_SubstringEqualString)
addTo(&precommitTests, "SubstringEquatable", run_SubstringEquatable)
addTo(&precommitTests, "SubstringFromLongString", run_SubstringFromLongString)
addTo(&precommitTests, "SubstringFromLongStringGeneric", run_SubstringFromLongStringGeneric)
addTo(&precommitTests, "SuffixAnyCollection", run_SuffixAnyCollection)
addTo(&precommitTests, "SuffixAnyCollectionLazy", run_SuffixAnyCollectionLazy)
addTo(&precommitTests, "SuffixAnySeqCRangeIter", run_SuffixAnySeqCRangeIter)
addTo(&precommitTests, "SuffixAnySeqCRangeIterLazy", run_SuffixAnySeqCRangeIterLazy)
addTo(&precommitTests, "SuffixAnySeqCntRange", run_SuffixAnySeqCntRange)
addTo(&precommitTests, "SuffixAnySeqCntRangeLazy", run_SuffixAnySeqCntRangeLazy)
addTo(&precommitTests, "SuffixAnySequence", run_SuffixAnySequence)
addTo(&precommitTests, "SuffixAnySequenceLazy", run_SuffixAnySequenceLazy)
addTo(&precommitTests, "SuffixArray", run_SuffixArray)
addTo(&precommitTests, "SuffixArrayLazy", run_SuffixArrayLazy)
addTo(&precommitTests, "SuffixCountableRange", run_SuffixCountableRange)
addTo(&precommitTests, "SuffixCountableRangeLazy", run_SuffixCountableRangeLazy)
addTo(&precommitTests, "SuffixSequence", run_SuffixSequence)
addTo(&precommitTests, "SuffixSequenceLazy", run_SuffixSequenceLazy)
addTo(&precommitTests, "SuperChars", run_SuperChars)
addTo(&precommitTests, "TwoSum", run_TwoSum)
addTo(&precommitTests, "TypeFlood", run_TypeFlood)
addTo(&precommitTests, "UTF8Decode", run_UTF8Decode)
addTo(&precommitTests, "Walsh", run_Walsh)
addTo(&precommitTests, "XorLoop", run_XorLoop)
addTo(&precommitTests, "accessGlobal", run_accessGlobal)
addTo(&precommitTests, "accessInMatSet", run_accessInMatSet)
addTo(&precommitTests, "accessIndependent", run_accessIndependent)

// Other tests
addTo(&otherTests, "Ackermann", run_Ackermann)
addTo(&otherTests, "ExistentialTestArrayConditionalShift_ClassValueBuffer1", run_ExistentialTestArrayConditionalShift_ClassValueBuffer1)
addTo(&otherTests, "ExistentialTestArrayConditionalShift_ClassValueBuffer2", run_ExistentialTestArrayConditionalShift_ClassValueBuffer2)
addTo(&otherTests, "ExistentialTestArrayConditionalShift_ClassValueBuffer3", run_ExistentialTestArrayConditionalShift_ClassValueBuffer3)
addTo(&otherTests, "ExistentialTestArrayConditionalShift_ClassValueBuffer4", run_ExistentialTestArrayConditionalShift_ClassValueBuffer4)
addTo(&otherTests, "ExistentialTestArrayConditionalShift_IntValueBuffer0", run_ExistentialTestArrayConditionalShift_IntValueBuffer0)
addTo(&otherTests, "ExistentialTestArrayConditionalShift_IntValueBuffer1", run_ExistentialTestArrayConditionalShift_IntValueBuffer1)
addTo(&otherTests, "ExistentialTestArrayConditionalShift_IntValueBuffer2", run_ExistentialTestArrayConditionalShift_IntValueBuffer2)
addTo(&otherTests, "ExistentialTestArrayConditionalShift_IntValueBuffer3", run_ExistentialTestArrayConditionalShift_IntValueBuffer3)
addTo(&otherTests, "ExistentialTestArrayConditionalShift_IntValueBuffer4", run_ExistentialTestArrayConditionalShift_IntValueBuffer4)
addTo(&otherTests, "ExistentialTestArrayMutating_ClassValueBuffer1", run_ExistentialTestArrayMutating_ClassValueBuffer1)
addTo(&otherTests, "ExistentialTestArrayMutating_ClassValueBuffer2", run_ExistentialTestArrayMutating_ClassValueBuffer2)
addTo(&otherTests, "ExistentialTestArrayMutating_ClassValueBuffer3", run_ExistentialTestArrayMutating_ClassValueBuffer3)
addTo(&otherTests, "ExistentialTestArrayMutating_ClassValueBuffer4", run_ExistentialTestArrayMutating_ClassValueBuffer4)
addTo(&otherTests, "ExistentialTestArrayMutating_IntValueBuffer0", run_ExistentialTestArrayMutating_IntValueBuffer0)
addTo(&otherTests, "ExistentialTestArrayMutating_IntValueBuffer1", run_ExistentialTestArrayMutating_IntValueBuffer1)
addTo(&otherTests, "ExistentialTestArrayMutating_IntValueBuffer2", run_ExistentialTestArrayMutating_IntValueBuffer2)
addTo(&otherTests, "ExistentialTestArrayMutating_IntValueBuffer3", run_ExistentialTestArrayMutating_IntValueBuffer3)
addTo(&otherTests, "ExistentialTestArrayMutating_IntValueBuffer4", run_ExistentialTestArrayMutating_IntValueBuffer4)
addTo(&otherTests, "ExistentialTestArrayOneMethodCall_ClassValueBuffer1", run_ExistentialTestArrayOneMethodCall_ClassValueBuffer1)
addTo(&otherTests, "ExistentialTestArrayOneMethodCall_ClassValueBuffer2", run_ExistentialTestArrayOneMethodCall_ClassValueBuffer2)
addTo(&otherTests, "ExistentialTestArrayOneMethodCall_ClassValueBuffer3", run_ExistentialTestArrayOneMethodCall_ClassValueBuffer3)
addTo(&otherTests, "ExistentialTestArrayOneMethodCall_ClassValueBuffer4", run_ExistentialTestArrayOneMethodCall_ClassValueBuffer4)
addTo(&otherTests, "ExistentialTestArrayOneMethodCall_IntValueBuffer0", run_ExistentialTestArrayOneMethodCall_IntValueBuffer0)
addTo(&otherTests, "ExistentialTestArrayOneMethodCall_IntValueBuffer1", run_ExistentialTestArrayOneMethodCall_IntValueBuffer1)
addTo(&otherTests, "ExistentialTestArrayOneMethodCall_IntValueBuffer2", run_ExistentialTestArrayOneMethodCall_IntValueBuffer2)
addTo(&otherTests, "ExistentialTestArrayOneMethodCall_IntValueBuffer3", run_ExistentialTestArrayOneMethodCall_IntValueBuffer3)
addTo(&otherTests, "ExistentialTestArrayOneMethodCall_IntValueBuffer4", run_ExistentialTestArrayOneMethodCall_IntValueBuffer4)
addTo(&otherTests, "ExistentialTestArrayShift_ClassValueBuffer1", run_ExistentialTestArrayShift_ClassValueBuffer1)
addTo(&otherTests, "ExistentialTestArrayShift_ClassValueBuffer2", run_ExistentialTestArrayShift_ClassValueBuffer2)
addTo(&otherTests, "ExistentialTestArrayShift_ClassValueBuffer3", run_ExistentialTestArrayShift_ClassValueBuffer3)
addTo(&otherTests, "ExistentialTestArrayShift_ClassValueBuffer4", run_ExistentialTestArrayShift_ClassValueBuffer4)
addTo(&otherTests, "ExistentialTestArrayShift_IntValueBuffer0", run_ExistentialTestArrayShift_IntValueBuffer0)
addTo(&otherTests, "ExistentialTestArrayShift_IntValueBuffer1", run_ExistentialTestArrayShift_IntValueBuffer1)
addTo(&otherTests, "ExistentialTestArrayShift_IntValueBuffer2", run_ExistentialTestArrayShift_IntValueBuffer2)
addTo(&otherTests, "ExistentialTestArrayShift_IntValueBuffer3", run_ExistentialTestArrayShift_IntValueBuffer3)
addTo(&otherTests, "ExistentialTestArrayShift_IntValueBuffer4", run_ExistentialTestArrayShift_IntValueBuffer4)
addTo(&otherTests, "ExistentialTestArrayTwoMethodCalls_ClassValueBuffer1", run_ExistentialTestArrayTwoMethodCalls_ClassValueBuffer1)
addTo(&otherTests, "ExistentialTestArrayTwoMethodCalls_ClassValueBuffer2", run_ExistentialTestArrayTwoMethodCalls_ClassValueBuffer2)
addTo(&otherTests, "ExistentialTestArrayTwoMethodCalls_ClassValueBuffer3", run_ExistentialTestArrayTwoMethodCalls_ClassValueBuffer3)
addTo(&otherTests, "ExistentialTestArrayTwoMethodCalls_ClassValueBuffer4", run_ExistentialTestArrayTwoMethodCalls_ClassValueBuffer4)
addTo(&otherTests, "ExistentialTestArrayTwoMethodCalls_IntValueBuffer0", run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer0)
addTo(&otherTests, "ExistentialTestArrayTwoMethodCalls_IntValueBuffer1", run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer1)
addTo(&otherTests, "ExistentialTestArrayTwoMethodCalls_IntValueBuffer2", run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer2)
addTo(&otherTests, "ExistentialTestArrayTwoMethodCalls_IntValueBuffer3", run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer3)
addTo(&otherTests, "ExistentialTestArrayTwoMethodCalls_IntValueBuffer4", run_ExistentialTestArrayTwoMethodCalls_IntValueBuffer4)
addTo(&otherTests, "ExistentialTestMutatingAndNonMutating_ClassValueBuffer1", run_ExistentialTestMutatingAndNonMutating_ClassValueBuffer1)
addTo(&otherTests, "ExistentialTestMutatingAndNonMutating_ClassValueBuffer2", run_ExistentialTestMutatingAndNonMutating_ClassValueBuffer2)
addTo(&otherTests, "ExistentialTestMutatingAndNonMutating_ClassValueBuffer3", run_ExistentialTestMutatingAndNonMutating_ClassValueBuffer3)
addTo(&otherTests, "ExistentialTestMutatingAndNonMutating_ClassValueBuffer4", run_ExistentialTestMutatingAndNonMutating_ClassValueBuffer4)
addTo(&otherTests, "ExistentialTestMutatingAndNonMutating_IntValueBuffer0", run_ExistentialTestMutatingAndNonMutating_IntValueBuffer0)
addTo(&otherTests, "ExistentialTestMutatingAndNonMutating_IntValueBuffer1", run_ExistentialTestMutatingAndNonMutating_IntValueBuffer1)
addTo(&otherTests, "ExistentialTestMutatingAndNonMutating_IntValueBuffer2", run_ExistentialTestMutatingAndNonMutating_IntValueBuffer2)
addTo(&otherTests, "ExistentialTestMutatingAndNonMutating_IntValueBuffer3", run_ExistentialTestMutatingAndNonMutating_IntValueBuffer3)
addTo(&otherTests, "ExistentialTestMutatingAndNonMutating_IntValueBuffer4", run_ExistentialTestMutatingAndNonMutating_IntValueBuffer4)
addTo(&otherTests, "ExistentialTestMutating_ClassValueBuffer1", run_ExistentialTestMutating_ClassValueBuffer1)
addTo(&otherTests, "ExistentialTestMutating_ClassValueBuffer2", run_ExistentialTestMutating_ClassValueBuffer2)
addTo(&otherTests, "ExistentialTestMutating_ClassValueBuffer3", run_ExistentialTestMutating_ClassValueBuffer3)
addTo(&otherTests, "ExistentialTestMutating_ClassValueBuffer4", run_ExistentialTestMutating_ClassValueBuffer4)
addTo(&otherTests, "ExistentialTestMutating_IntValueBuffer0", run_ExistentialTestMutating_IntValueBuffer0)
addTo(&otherTests, "ExistentialTestMutating_IntValueBuffer1", run_ExistentialTestMutating_IntValueBuffer1)
addTo(&otherTests, "ExistentialTestMutating_IntValueBuffer2", run_ExistentialTestMutating_IntValueBuffer2)
addTo(&otherTests, "ExistentialTestMutating_IntValueBuffer3", run_ExistentialTestMutating_IntValueBuffer3)
addTo(&otherTests, "ExistentialTestMutating_IntValueBuffer4", run_ExistentialTestMutating_IntValueBuffer4)
addTo(&otherTests, "ExistentialTestOneMethodCall_ClassValueBuffer1", run_ExistentialTestOneMethodCall_ClassValueBuffer1)
addTo(&otherTests, "ExistentialTestOneMethodCall_ClassValueBuffer2", run_ExistentialTestOneMethodCall_ClassValueBuffer2)
addTo(&otherTests, "ExistentialTestOneMethodCall_ClassValueBuffer3", run_ExistentialTestOneMethodCall_ClassValueBuffer3)
addTo(&otherTests, "ExistentialTestOneMethodCall_ClassValueBuffer4", run_ExistentialTestOneMethodCall_ClassValueBuffer4)
addTo(&otherTests, "ExistentialTestOneMethodCall_IntValueBuffer0", run_ExistentialTestOneMethodCall_IntValueBuffer0)
addTo(&otherTests, "ExistentialTestOneMethodCall_IntValueBuffer1", run_ExistentialTestOneMethodCall_IntValueBuffer1)
addTo(&otherTests, "ExistentialTestOneMethodCall_IntValueBuffer2", run_ExistentialTestOneMethodCall_IntValueBuffer2)
addTo(&otherTests, "ExistentialTestOneMethodCall_IntValueBuffer3", run_ExistentialTestOneMethodCall_IntValueBuffer3)
addTo(&otherTests, "ExistentialTestOneMethodCall_IntValueBuffer4", run_ExistentialTestOneMethodCall_IntValueBuffer4)
addTo(&otherTests, "ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer1", run_ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer1)
addTo(&otherTests, "ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer2", run_ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer2)
addTo(&otherTests, "ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer3", run_ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer3)
addTo(&otherTests, "ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer4", run_ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer4)
addTo(&otherTests, "ExistentialTestPassExistentialOneMethodCall_IntValueBuffer0", run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer0)
addTo(&otherTests, "ExistentialTestPassExistentialOneMethodCall_IntValueBuffer1", run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer1)
addTo(&otherTests, "ExistentialTestPassExistentialOneMethodCall_IntValueBuffer2", run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer2)
addTo(&otherTests, "ExistentialTestPassExistentialOneMethodCall_IntValueBuffer3", run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer3)
addTo(&otherTests, "ExistentialTestPassExistentialOneMethodCall_IntValueBuffer4", run_ExistentialTestPassExistentialOneMethodCall_IntValueBuffer4)
addTo(&otherTests, "ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer1", run_ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer1)
addTo(&otherTests, "ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer2", run_ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer2)
addTo(&otherTests, "ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer3", run_ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer3)
addTo(&otherTests, "ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer4", run_ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer4)
addTo(&otherTests, "ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer0", run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer0)
addTo(&otherTests, "ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer1", run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer1)
addTo(&otherTests, "ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer2", run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer2)
addTo(&otherTests, "ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer3", run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer3)
addTo(&otherTests, "ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer4", run_ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer4)
addTo(&otherTests, "ExistentialTestTwoMethodCalls_ClassValueBuffer1", run_ExistentialTestTwoMethodCalls_ClassValueBuffer1)
addTo(&otherTests, "ExistentialTestTwoMethodCalls_ClassValueBuffer2", run_ExistentialTestTwoMethodCalls_ClassValueBuffer2)
addTo(&otherTests, "ExistentialTestTwoMethodCalls_ClassValueBuffer3", run_ExistentialTestTwoMethodCalls_ClassValueBuffer3)
addTo(&otherTests, "ExistentialTestTwoMethodCalls_ClassValueBuffer4", run_ExistentialTestTwoMethodCalls_ClassValueBuffer4)
addTo(&otherTests, "ExistentialTestTwoMethodCalls_IntValueBuffer0", run_ExistentialTestTwoMethodCalls_IntValueBuffer0)
addTo(&otherTests, "ExistentialTestTwoMethodCalls_IntValueBuffer1", run_ExistentialTestTwoMethodCalls_IntValueBuffer1)
addTo(&otherTests, "ExistentialTestTwoMethodCalls_IntValueBuffer2", run_ExistentialTestTwoMethodCalls_IntValueBuffer2)
addTo(&otherTests, "ExistentialTestTwoMethodCalls_IntValueBuffer3", run_ExistentialTestTwoMethodCalls_IntValueBuffer3)
addTo(&otherTests, "ExistentialTestTwoMethodCalls_IntValueBuffer4", run_ExistentialTestTwoMethodCalls_IntValueBuffer4)
addTo(&otherTests, "Fibonacci", run_Fibonacci)
addTo(&otherTests, "HashQuadratic", run_HashQuadratic)

// String tests, an extended benchmark suite exercising finer-granularity
// behavior of our Strings.
addTo(&stringTests, "StringWalk_ascii_characters", run_StringWalk_ascii_characters)
addTo(&stringTests, "StringWalk_ascii_characters_Backwards", run_StringWalk_ascii_characters_Backwards)
addTo(&stringTests, "StringWalk_ascii_unicodeScalars", run_StringWalk_ascii_unicodeScalars)
addTo(&stringTests, "StringWalk_ascii_unicodeScalars_Backwards", run_StringWalk_ascii_unicodeScalars_Backwards)
addTo(&stringTests, "StringWalk_chinese_characters", run_StringWalk_chinese_characters)
addTo(&stringTests, "StringWalk_chinese_characters_Backwards", run_StringWalk_chinese_characters_Backwards)
addTo(&stringTests, "StringWalk_chinese_unicodeScalars", run_StringWalk_chinese_unicodeScalars)
addTo(&stringTests, "StringWalk_chinese_unicodeScalars_Backwards", run_StringWalk_chinese_unicodeScalars_Backwards)
addTo(&stringTests, "StringWalk_japanese_characters", run_StringWalk_japanese_characters)
addTo(&stringTests, "StringWalk_japanese_characters_Backwards", run_StringWalk_japanese_characters_Backwards)
addTo(&stringTests, "StringWalk_japanese_unicodeScalars", run_StringWalk_japanese_unicodeScalars)
addTo(&stringTests, "StringWalk_japanese_unicodeScalars_Backwards", run_StringWalk_japanese_unicodeScalars_Backwards)
addTo(&stringTests, "StringWalk_korean_characters", run_StringWalk_korean_characters)
addTo(&stringTests, "StringWalk_korean_characters_Backwards", run_StringWalk_korean_characters_Backwards)
addTo(&stringTests, "StringWalk_korean_unicodeScalars", run_StringWalk_korean_unicodeScalars)
addTo(&stringTests, "StringWalk_korean_unicodeScalars_Backwards", run_StringWalk_korean_unicodeScalars_Backwards)
addTo(&stringTests, "StringWalk_punctuatedJapanese_characters", run_StringWalk_punctuatedJapanese_characters)
addTo(&stringTests, "StringWalk_punctuatedJapanese_characters_Backwards", run_StringWalk_punctuatedJapanese_characters_Backwards)
addTo(&stringTests, "StringWalk_punctuatedJapanese_unicodeScalars", run_StringWalk_punctuatedJapanese_unicodeScalars)
addTo(&stringTests, "StringWalk_punctuatedJapanese_unicodeScalars_Backwards", run_StringWalk_punctuatedJapanese_unicodeScalars_Backwards)
addTo(&stringTests, "StringWalk_punctuated_characters", run_StringWalk_punctuated_characters)
addTo(&stringTests, "StringWalk_punctuated_characters_Backwards", run_StringWalk_punctuated_characters_Backwards)
addTo(&stringTests, "StringWalk_punctuated_unicodeScalars", run_StringWalk_punctuated_unicodeScalars)
addTo(&stringTests, "StringWalk_punctuated_unicodeScalars_Backwards", run_StringWalk_punctuated_unicodeScalars_Backwards)
addTo(&stringTests, "StringWalk_russian_characters", run_StringWalk_russian_characters)
addTo(&stringTests, "StringWalk_russian_characters_Backwards", run_StringWalk_russian_characters_Backwards)
addTo(&stringTests, "StringWalk_russian_unicodeScalars", run_StringWalk_russian_unicodeScalars)
addTo(&stringTests, "StringWalk_russian_unicodeScalars_Backwards", run_StringWalk_russian_unicodeScalars_Backwards)
addTo(&stringTests, "StringWalk_tweet_characters", run_StringWalk_tweet_characters)
addTo(&stringTests, "StringWalk_tweet_characters_Backwards", run_StringWalk_tweet_characters_Backwards)
addTo(&stringTests, "StringWalk_tweet_unicodeScalars", run_StringWalk_tweet_unicodeScalars)
addTo(&stringTests, "StringWalk_tweet_unicodeScalars_Backwards", run_StringWalk_tweet_unicodeScalars_Backwards)
addTo(&stringTests, "StringWalk_utf16_characters", run_StringWalk_utf16_characters)
addTo(&stringTests, "StringWalk_utf16_characters_Backwards", run_StringWalk_utf16_characters_Backwards)
addTo(&stringTests, "StringWalk_utf16_unicodeScalars", run_StringWalk_utf16_unicodeScalars)
addTo(&stringTests, "StringWalk_utf16_unicodeScalars_Backwards", run_StringWalk_utf16_unicodeScalars_Backwards)

main()
