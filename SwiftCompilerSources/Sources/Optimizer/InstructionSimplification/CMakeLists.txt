# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for Swift project authors

swift_compiler_sources(Optimizer
  SimplifyAllocRefDynamic.swift
  SimplifyAllocStack.swift
  SimplifyApply.swift
  SimplifyBeginBorrow.swift
  SimplifyBeginCOWMutation.swift
  SimplifyBorrow.swift
  SimplifyBranch.swift
  SimplifyBuiltin.swift
  SimplifyCheckedCast.swift
  SimplifyClassifyBridgeObject.swift
  SimplifyCondBranch.swift
  SimplifyCondFail.swift
  SimplifyConvertEscapeToNoEscape.swift
  SimplifyCopyBlock.swift
  SimplifyCopyValue.swift
  SimplifyDebugStep.swift
  SimplifyDestroyValue.swift
  SimplifyDestructure.swift
  SimplifyEndCOWMutationAddr.swift
  SimplifyExplicitCopy.swift
  SimplifyFixLifetime.swift
  SimplifyGlobalValue.swift
  SimplifyInitEnumDataAddr.swift
  SimplifyKeyPath.swift
  SimplifyLoad.swift
  SimplifyLoadBorrow.swift
  SimplifyMarkDependence.swift
  SimplifyMoveValue.swift
  SimplifyPartialApply.swift
  SimplifyPointerToAddress.swift
  SimplifyRefCasts.swift
  SimplifyRetainReleaseValue.swift
  SimplifyStrongRetainRelease.swift
  SimplifyStruct.swift
  SimplifyStructExtract.swift
  SimplifySwitchEnum.swift
  SimplifyTuple.swift
  SimplifyTupleExtract.swift
  SimplifyTypeValue.swift
  SimplifyUncheckedAddrCast.swift
  SimplifyUncheckedEnumData.swift
  SimplifyValueToBridgeObject.swift
  SimplifyWitnessMethod.swift)
