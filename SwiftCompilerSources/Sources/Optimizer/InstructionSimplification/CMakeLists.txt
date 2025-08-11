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
  SimplifyBeginAndLoadBorrow.swift
  SimplifyBeginCOWMutation.swift
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
  SimplifyFixLifetime.swift
  SimplifyGlobalValue.swift
  SimplifyInitEnumDataAddr.swift
  SimplifyKeyPath.swift
  SimplifyLoad.swift
  SimplifyMarkDependence.swift
  SimplifyMisc.swift
  SimplifyPartialApply.swift
  SimplifyPointerToAddress.swift
  SimplifyRefCasts.swift
  SimplifyRetainReleaseValue.swift
  SimplifyStrongRetainRelease.swift
  SimplifyStructExtract.swift
  SimplifySwitchEnum.swift
  SimplifyTuple.swift
  SimplifyTupleExtract.swift
  SimplifyUncheckedAddrCast.swift
  SimplifyUncheckedEnumData.swift
  SimplifyValueToBridgeObject.swift
  SimplifyWitnessMethod.swift)
