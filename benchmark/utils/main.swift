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
import DictionarySubscriptDefault
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
import RangeIteration
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
import StringComparison
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
private func registerBenchmark<
  S : Sequence
>(_ infos: S) where S.Element == BenchmarkInfo {
  registeredBenchmarks.append(contentsOf: infos)
}

registerBenchmark(Ackermann)
registerBenchmark(AngryPhonebook)
registerBenchmark(AnyHashableWithAClass)
registerBenchmark(Array2D)
registerBenchmark(ArrayAppend)
registerBenchmark(ArrayInClass)
registerBenchmark(ArrayLiteral)
registerBenchmark(ArrayOfGenericPOD)
registerBenchmark(ArrayOfGenericRef)
registerBenchmark(ArrayOfPOD)
registerBenchmark(ArrayOfRef)
registerBenchmark(ArraySetElement)
registerBenchmark(ArraySubscript)
registerBenchmark(BitCount)
registerBenchmark(ByteSwap)
registerBenchmark(CString)
registerBenchmark(Calculator)
registerBenchmark(CaptureProp)
registerBenchmark(CharacterLiteralsLarge)
registerBenchmark(CharacterLiteralsSmall)
registerBenchmark(Chars)
registerBenchmark(ClassArrayGetter)
registerBenchmark(DeadArray)
registerBenchmark(Dictionary)
registerBenchmark(Dictionary2)
registerBenchmark(Dictionary3)
registerBenchmark(DictionaryBridge)
registerBenchmark(DictionaryGroup)
registerBenchmark(DictionaryLiteral)
registerBenchmark(DictionaryRemove)
registerBenchmark(DictionarySubscriptDefault)
registerBenchmark(DictionarySwap)
registerBenchmark(DropFirst)
registerBenchmark(DropLast)
registerBenchmark(DropWhile)
registerBenchmark(ErrorHandling)
registerBenchmark(Exclusivity)
registerBenchmark(ExistentialPerformance)
registerBenchmark(Fibonacci)
registerBenchmark(Hanoi)
registerBenchmark(HashTest)
registerBenchmark(HashQuadratic)
registerBenchmark(Histogram)
registerBenchmark(IntegrateTest)
registerBenchmark(IterateData)
registerBenchmark(Join)
registerBenchmark(LazyFilter)
registerBenchmark(LinkedList)
registerBenchmark(MapReduce)
registerBenchmark(Memset)
registerBenchmark(MonteCarloE)
registerBenchmark(MonteCarloPi)
registerBenchmark(NSDictionaryCastToSwift)
registerBenchmark(NSErrorTest)
registerBenchmark(NSStringConversion)
registerBenchmark(NopDeinit)
registerBenchmark(ObjectAllocation)
registerBenchmark(ObjectiveCBridging)
registerBenchmark(ObjectiveCBridgingStubs)
registerBenchmark(ObjectiveCNoBridgingStubs)
registerBenchmark(ObserverClosure)
registerBenchmark(ObserverForwarderStruct)
registerBenchmark(ObserverPartiallyAppliedMethod)
registerBenchmark(ObserverUnappliedMethod)
registerBenchmark(OpenClose)
registerBenchmark(Phonebook)
registerBenchmark(PolymorphicCalls)
registerBenchmark(PopFront)
registerBenchmark(PopFrontArrayGeneric)
registerBenchmark(Prefix)
registerBenchmark(PrefixWhile)
registerBenchmark(Prims)
registerBenchmark(PrimsSplit)
registerBenchmark(ProtocolDispatch)
registerBenchmark(ProtocolDispatch2)
registerBenchmark(RC4Test)
registerBenchmark(RGBHistogram)
registerBenchmark(RangeAssignment)
registerBenchmark(RangeIteration)
registerBenchmark(RecursiveOwnedParameter)
registerBenchmark(ReduceInto)
registerBenchmark(ReversedCollections)
registerBenchmark(SetTests)
registerBenchmark(SevenBoom)
registerBenchmark(Sim2DArray)
registerBenchmark(SortLargeExistentials)
registerBenchmark(SortLettersInPlace)
registerBenchmark(SortStrings)
registerBenchmark(StackPromo)
registerBenchmark(StaticArrayTest)
registerBenchmark(StrComplexWalk)
registerBenchmark(StrToInt)
registerBenchmark(StringBuilder)
registerBenchmark(StringComparison)
registerBenchmark(StringEdits)
registerBenchmark(StringEnum)
registerBenchmark(StringInterpolation)
registerBenchmark(StringMatch)
registerBenchmark(StringTests)
registerBenchmark(StringWalk)
registerBenchmark(SubstringTest)
registerBenchmark(Suffix)
registerBenchmark(SuperChars)
registerBenchmark(TwoSum)
registerBenchmark(TypeFlood)
registerBenchmark(UTF8Decode)
registerBenchmark(Walsh)
registerBenchmark(XorLoop)

main()
