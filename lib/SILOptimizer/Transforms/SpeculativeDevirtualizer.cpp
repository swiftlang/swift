//===--- SpeculativeDevirtualizer.cpp - Speculatively devirtualize calls --===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Speculatively devirtualizes witness- and class-method calls into direct
// calls.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-speculative-devirtualizer"
#include "swift/Basic/DemangleWrappers.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SILOptimizer/Analysis/ClassHierarchyAnalysis.h"
#include "swift/SILOptimizer/Utils/Generics.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Devirtualize.h"
#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "swift/AST/ASTContext.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"
using namespace swift;

static const char *Functions[] = {
"_TZFsoi2aauRxs7BooleanrFzTxKzT_Sb_Sb",
"_TFs9_fastPathuRxs7BooleanrFxSb",
"_TFs9_slowPathuRxs7BooleanrFxSb",
"_TZFsoi2oouRxs7BooleanrFzTxKzT_Sb_Sb",
"_TFs11numericCastu0_Rxs14_SignedInteger_S_rFxq_",
"_TFs11numericCastu0_Rxs14_SignedInteger_S_rFxq_",
"_TFs11numericCastu0_Rxs14_SignedInteger_S_rFxq_",
"_TFs11numericCastu0_Rxs14_SignedInteger_S_rFxq_",
"_TFs11numericCastu0_Rxs14_SignedInteger_S_rFxq_",
"_TFs11_branchHintuRxs7BooleanrFTx8expectedSb_Sb",
"_TFSbCuRxs7BooleanrfxSb",
"_TZFsop1nuRxs7BooleanrFxSb",
"_TZFsoi2aau0_Rxs7Boolean_S_rFzTxKzT_q__Sb",
"_TZFsoi2aau0_Rxs7Boolean_S_rFzTxKzT_q__Sb",
"_TZFsoi2oou0_Rxs7Boolean_S_rFzTxKzT_q__Sb",
"_TZFsoi2oou0_Rxs7Boolean_S_rFzTxKzT_q__Sb",
"_TFs11numericCastu0_Rxs14_SignedInteger_s15UnsignedIntegerrFxq_",
"_TFs11numericCastu0_Rxs14_SignedInteger_s15UnsignedIntegerrFxq_",
"_TFs11numericCastu0_Rxs14_SignedInteger_s15UnsignedIntegerrFxq_",
"_TFs11numericCastu0_Rxs14_SignedInteger_s15UnsignedIntegerrFxq_",
"_TFs11numericCastu0_Rxs14_SignedInteger_s15UnsignedIntegerrFxq_",
"_TFs10_introSortuRxs17MutableCollectionwx5Indexs17RandomAccessIndexWx8Iterator7Element_s10ComparablerFTRx8subRangeGVs5RangewxS0___T_",
"_TFs10_introSortuRxs17MutableCollectionwx5Indexs17RandomAccessIndexWx8Iterator7Element_s10ComparablerFTRx8subRangeGVs5RangewxS0___T_",
"_TFs10_introSortuRxs17MutableCollectionwx5Indexs17RandomAccessIndexWx8Iterator7Element_s10ComparablerFTRx8subRangeGVs5RangewxS0___T_",
"_TFs10_introSortuRxs17MutableCollectionwx5Indexs17RandomAccessIndexWx8Iterator7Element_s10ComparablerFTRx8subRangeGVs5RangewxS0___T_",
"_TFs10_introSortuRxs17MutableCollectionwx5Indexs17RandomAccessIndexWx8Iterator7Element_s10ComparablerFTRx8subRangeGVs5RangewxS0___T_",
"_TFs10_introSortuRxs17MutableCollectionwx5Indexs17RandomAccessIndexrFTRx8subRangeGVs5RangewxS0__15isOrderedBeforeFTWx8Iterator7Element_WxS3_S4___Sb_T_",
"_TFs10_introSortuRxs17MutableCollectionwx5Indexs17RandomAccessIndexrFTRx8subRangeGVs5RangewxS0__15isOrderedBeforeFTWx8Iterator7Element_WxS3_S4___Sb_T_",
"_TFs10_introSortuRxs17MutableCollectionwx5Indexs17RandomAccessIndexrFTRx8subRangeGVs5RangewxS0__15isOrderedBeforeFTWx8Iterator7Element_WxS3_S4___Sb_T_",
"_TFs10_introSortuRxs17MutableCollectionwx5Indexs17RandomAccessIndexrFTRx8subRangeGVs5RangewxS0__15isOrderedBeforeFTWx8Iterator7Element_WxS3_S4___Sb_T_",
"_TFs10_introSortuRxs17MutableCollectionwx5Indexs17RandomAccessIndexrFTRx8subRangeGVs5RangewxS0__15isOrderedBeforeFTWx8Iterator7Element_WxS3_S4___Sb_T_",
"_TFs11numericCastu0_Rxs15UnsignedInteger_S_rFxq_",
"_TFs11numericCastu0_Rxs15UnsignedInteger_S_rFxq_",
"_TFs11numericCastu0_Rxs15UnsignedInteger_S_rFxq_",
"_TFs11numericCastu0_Rxs15UnsignedInteger_S_rFxq_",
"_TFs11numericCastu0_Rxs15UnsignedInteger_S_rFxq_",
"_TFs11numericCastu0_Rxs15UnsignedInteger_s14_SignedIntegerrFxq_",
"_TFs11numericCastu0_Rxs15UnsignedInteger_s14_SignedIntegerrFxq_",
"_TFs11numericCastu0_Rxs15UnsignedInteger_s14_SignedIntegerrFxq_",
"_TFs11numericCastu0_Rxs15UnsignedInteger_s14_SignedIntegerrFxq_",
"_TFs11numericCastu0_Rxs15UnsignedInteger_s14_SignedIntegerrFxq_",
"_TFSSCuRxs14_SignedIntegerrfxSS",
"_TFSSCuRxs14_SignedIntegerrfxSS",
"_TFSSCuRxs14_SignedIntegerrfxSS",
"_TFSSCuRxs14_SignedIntegerrfxSS",
"_TFSSCuRxs14_SignedIntegerrfxSS",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_",
"_TFFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_L_13printTypeNamefPMP_T_",
"_TFFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_L_13printTypeNamefPMP_T_",
"_TFSS5writeuRxs12OutputStreamrfT2toRx_T_",
"_TFSS5writeuRxs12OutputStreamrfT2toRx_T_",
"_TFs19_dumpPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq__T_",
"_TFs19_dumpPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq__T_",
"_TFs19_dumpPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq__T_",
"_TFs19_dumpPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq__T_",
"_TFs19_dumpPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq__T_",
"_TFs19_dumpPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq__T_",
"_TFs19_dumpPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq__T_",
"_TFs19_dumpPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq__T_",
"_TFs19_dumpPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq__T_",
"_TFs19_dumpPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq__T_",
"_TFs19_dumpPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq__T_",
"_TFs19_dumpPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq__T_",
"_TFs19_dumpPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq__T_",
"_TFs19_dumpPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq__T_",
"_TFs19_dumpPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq__T_",
"_TFs19_dumpPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq__T_",
"_TFVs9Character5writeuRxs12OutputStreamrfT2toRx_T_",
"_TFVs9Character5writeuRxs12OutputStreamrfT2toRx_T_",
"_TFSc5writeuRxs12OutputStreamrfT2toRx_T_",
"_TFSc5writeuRxs12OutputStreamrfT2toRx_T_",
"_TFVs10_TeeStream5writefSST_",
"_TFVs10_TeeStream5writefSST_",
"_TFVs10_TeeStream5writefSST_",
"_TFVs10_TeeStream5writefSST_",
"_TFVs10_TeeStream5_lockfT_T_",
"_TFVs10_TeeStream5_lockfT_T_",
"_TFVs10_TeeStream5_lockfT_T_",
"_TFVs10_TeeStream5_lockfT_T_",
"_TFVs10_TeeStream7_unlockfT_T_",
"_TFVs10_TeeStream7_unlockfT_T_",
"_TFVs10_TeeStream7_unlockfT_T_",
"_TFVs10_TeeStream7_unlockfT_T_",
"_TFs6_printuRxs12OutputStreamrFTGSaP__9separatorSS10terminatorSS2toRx_T_",
"_TFs6_printuRxs12OutputStreamrFTGSaP__9separatorSS10terminatorSS2toRx_T_",
"_TFs6_printuRxs12OutputStreamrFTGSaP__9separatorSS10terminatorSS2toRx_T_",
"_TFs6_printuRxs12OutputStreamrFTGSaP__9separatorSS10terminatorSS2toRx_T_",
"_TFs6_printuRxs12OutputStreamrFTGSaP__9separatorSS10terminatorSS2toRx_T_",
"_TFs6_printuRxs12OutputStreamrFTGSaP__9separatorSS10terminatorSS2toRx_T_",
"_TFs11_debugPrintuRxs12OutputStreamrFTGSaP__9separatorSS10terminatorSS2toRx_T_",
"_TFs11_debugPrintuRxs12OutputStreamrFTGSaP__9separatorSS10terminatorSS2toRx_T_",
"_TFs11_debugPrintuRxs12OutputStreamrFTGSaP__9separatorSS10terminatorSS2toRx_T_",
"_TFs11_debugPrintuRxs12OutputStreamrFTGSaP__9separatorSS10terminatorSS2toRx_T_",
"_TFs11_debugPrintuRxs12OutputStreamrFTGSaP__9separatorSS10terminatorSS2toRx_T_",
"_TFs11_debugPrintuRxs12OutputStreamrFTGSaP__9separatorSS10terminatorSS2toRx_T_",
"_TFFs6_printuRxs12OutputStreamrFTGSaP__9separatorSS10terminatorSS2toRx_T_L_6$deferfT_T_",
"_TFFs6_printuRxs12OutputStreamrFTGSaP__9separatorSS10terminatorSS2toRx_T_L_6$deferfT_T_",
"_TFFs11_debugPrintuRxs12OutputStreamrFTGSaP__9separatorSS10terminatorSS2toRx_T_L_6$deferfT_T_",
"_TFFs11_debugPrintuRxs12OutputStreamrFTGSaP__9separatorSS10terminatorSS2toRx_T_L_6$deferfT_T_",
"_TFs4dumpu0_R_s12OutputStreamrFTx2toRq_4nameGSqSS_6indentSi8maxDepthSi8maxItemsSi_x",
"_TFs4dumpu0_R_s12OutputStreamrFTx2toRq_4nameGSqSS_6indentSi8maxDepthSi8maxItemsSi_x",
"_TFFs4dumpu0_R_s12OutputStreamrFTx2toRq_4nameGSqSS_6indentSi8maxDepthSi8maxItemsSi_xL_6$deferfT_T_",
"_TFFs4dumpu0_R_s12OutputStreamrFTx2toRq_4nameGSqSS_6indentSi8maxDepthSi8maxItemsSi_xL_6$deferfT_T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs14_dump_unlockeduRxs12OutputStreamrFTP_2toRx4nameGSqSS_6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs24_dumpSuperclass_unlockeduRxs12OutputStreamrFT6mirrorVs6Mirror2toRx6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs24_dumpSuperclass_unlockeduRxs12OutputStreamrFT6mirrorVs6Mirror2toRx6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs24_dumpSuperclass_unlockeduRxs12OutputStreamrFT6mirrorVs6Mirror2toRx6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs24_dumpSuperclass_unlockeduRxs12OutputStreamrFT6mirrorVs6Mirror2toRx6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs24_dumpSuperclass_unlockeduRxs12OutputStreamrFT6mirrorVs6Mirror2toRx6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs24_dumpSuperclass_unlockeduRxs12OutputStreamrFT6mirrorVs6Mirror2toRx6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs24_dumpSuperclass_unlockeduRxs12OutputStreamrFT6mirrorVs6Mirror2toRx6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs24_dumpSuperclass_unlockeduRxs12OutputStreamrFT6mirrorVs6Mirror2toRx6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs24_dumpSuperclass_unlockeduRxs12OutputStreamrFT6mirrorVs6Mirror2toRx6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs24_dumpSuperclass_unlockeduRxs12OutputStreamrFT6mirrorVs6Mirror2toRx6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs24_dumpSuperclass_unlockeduRxs12OutputStreamrFT6mirrorVs6Mirror2toRx6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs24_dumpSuperclass_unlockeduRxs12OutputStreamrFT6mirrorVs6Mirror2toRx6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs24_dumpSuperclass_unlockeduRxs12OutputStreamrFT6mirrorVs6Mirror2toRx6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs24_dumpSuperclass_unlockeduRxs12OutputStreamrFT6mirrorVs6Mirror2toRx6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs24_dumpSuperclass_unlockeduRxs12OutputStreamrFT6mirrorVs6Mirror2toRx6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs24_dumpSuperclass_unlockeduRxs12OutputStreamrFT6mirrorVs6Mirror2toRx6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs24_dumpSuperclass_unlockeduRxs12OutputStreamrFT6mirrorVs6Mirror2toRx6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs24_dumpSuperclass_unlockeduRxs12OutputStreamrFT6mirrorVs6Mirror2toRx6indentSi8maxDepthSi14maxItemCounterRSi12visitedItemsRGVs10DictionaryVs16ObjectIdentifierSi__T_",
"_TFs8_heapifyuRxs17MutableCollectionwx5Indexs17RandomAccessIndexrFTRx8subRangeGVs5RangewxS0__15isOrderedBeforeRFTWx8Iterator7Element_WxS3_S4___Sb_T_",
"_TFs8_heapifyuRxs17MutableCollectionwx5Indexs17RandomAccessIndexrFTRx8subRangeGVs5RangewxS0__15isOrderedBeforeRFTWx8Iterator7Element_WxS3_S4___Sb_T_",
"_TFs8_heapifyuRxs17MutableCollectionwx5Indexs17RandomAccessIndexrFTRx8subRangeGVs5RangewxS0__15isOrderedBeforeRFTWx8Iterator7Element_WxS3_S4___Sb_T_",
"_TFs8_heapifyuRxs17MutableCollectionwx5Indexs17RandomAccessIndexrFTRx8subRangeGVs5RangewxS0__15isOrderedBeforeRFTWx8Iterator7Element_WxS3_S4___Sb_T_",
"_TFs8_heapifyuRxs17MutableCollectionwx5Indexs17RandomAccessIndexrFTRx8subRangeGVs5RangewxS0__15isOrderedBeforeRFTWx8Iterator7Element_WxS3_S4___Sb_T_",
"_TFs8_heapifyuRxs17MutableCollectionwx5Indexs17RandomAccessIndexWx8Iterator7Element_s10ComparablerFTRx8subRangeGVs5RangewxS0___T_",
"_TFs8_heapifyuRxs17MutableCollectionwx5Indexs17RandomAccessIndexWx8Iterator7Element_s10ComparablerFTRx8subRangeGVs5RangewxS0___T_",
"_TFs8_heapifyuRxs17MutableCollectionwx5Indexs17RandomAccessIndexWx8Iterator7Element_s10ComparablerFTRx8subRangeGVs5RangewxS0___T_",
"_TFs8_heapifyuRxs17MutableCollectionwx5Indexs17RandomAccessIndexWx8Iterator7Element_s10ComparablerFTRx8subRangeGVs5RangewxS0___T_",
"_TFs8_heapifyuRxs17MutableCollectionwx5Indexs17RandomAccessIndexWx8Iterator7Element_s10ComparablerFTRx8subRangeGVs5RangewxS0___T_",
"_TFSSCuRxs15UnsignedIntegerrfxSS",
"_TFSSCuRxs15UnsignedIntegerrfxSS",
"_TFSSCuRxs15UnsignedIntegerrfxSS",
"_TFSSCuRxs15UnsignedIntegerrfxSS",
"_TFSSCuRxs15UnsignedIntegerrfxSS",
"_TFSSCuRxs14_SignedIntegerrfTx5radixSi9uppercaseSb_SS",
"_TFSSCuRxs14_SignedIntegerrfTx5radixSi9uppercaseSb_SS",
"_TFSSCuRxs14_SignedIntegerrfTx5radixSi9uppercaseSb_SS",
"_TFSSCuRxs14_SignedIntegerrfTx5radixSi9uppercaseSb_SS",
"_TFSSCuRxs14_SignedIntegerrfTx5radixSi9uppercaseSb_SS",
"_TFSSCuRxs15UnsignedIntegerrfTx5radixSi9uppercaseSb_SS",
"_TFSSCuRxs15UnsignedIntegerrfTx5radixSi9uppercaseSb_SS",
"_TFSSCuRxs15UnsignedIntegerrfTx5radixSi9uppercaseSb_SS",
"_TFSSCuRxs15UnsignedIntegerrfTx5radixSi9uppercaseSb_SS",
"_TFSSCuRxs15UnsignedIntegerrfTx5radixSi9uppercaseSb_SS",
};

static int startIdx = 78;
// Start: minIdx = 0: max index: 201
// static int endIdx = sizeof(Functions)/sizeof(Functions[0]);
static int endIdx = 79;
// 201 - bad (all bugs), 100 - bad ( bugs 72), 50 - bad (bugs 17), 25 - bad (bugs - 4)
// (25, 50) - bad (bugs 17)
// (25, 38) - bad (bugs 4)
// (38, 50) - bad (bugs 17)
// (38, 44) - bad (bugs 4)
// (44, 47) - bad (bugs 17)
// (47, 50) - bad (bugs 17)
// (46, 47) - bad (bugs 17) -> the problematic function is (46) _TFs20_adHocPrint_unlockedu0_R_s12OutputStreamrFTxVs6MirrorRq_12isDebugPrintSb_T_<Paste>
// which is Swift._adHocPrint_unlocked <A, B where B: Swift.OutputStream> (A, Swift.Mirror, inout B, isDebugPrint : Swift.Bool) -> () 
// (44, 46) - bad (bugs 4)
// (50, 100) - bad (bugs 72)
// (50, 75) - bad (bugs 17)
// (75, 81) - bad (bugs 72)
// (75, 78) - bad (bugs 26)
// (75, 76) - bad (bugs 17)
// (76, 78) - bad (bugs 18)
// (78, 81) - bad (bugs 66)
// (78, 80) - bad (bugs 65)
// (78, 79) - bad (bugs 65)
// problem in _TFSS5writeuRxs12OutputStreamrfT2toRx_T_
// Swift.String.write <A where A: Swift.OutputStream> (to : inout A) -> ()
// (87, 100) - bad (bugs 9)

static bool shouldProcessFunction(const char *Functions[], StringRef FuncName,
                                  int startIdx, int endIdx) {
  return true;
  for (auto i = startIdx, e = endIdx; i < e; ++i) {
    if (FuncName == Functions[i]) {
      llvm::dbgs() << "Should process function at index " << i
                   << ": minIdx = " << startIdx << ": max index: " << endIdx
                   << ":" << FuncName << "\n";
      return true;
    }
  }
  return false;
}

llvm::cl::opt<bool> SpecDevirtNonClassProtocols(
    "specdevirt-non-class-protocols", llvm::cl::init(true),
    llvm::cl::desc(
        "Enable speculative devirtualization of non-class protocols"));

// This is the limit for the number of subclasses (jump targets) that the
// speculative devirtualizer will try to predict.
static const int MaxNumSpeculativeTargets = 6;

STATISTIC(NumTargetsPredicted, "Number of monomorphic functions predicted");

// A utility function for cloning the apply instruction.
static FullApplySite CloneApply(FullApplySite AI, SILBuilder &Builder) {
  // Clone the Apply.
  Builder.setCurrentDebugScope(AI.getDebugScope());
  auto Args = AI.getArguments();
  SmallVector<SILValue, 8> Ret(Args.size());
  for (unsigned i = 0, e = Args.size(); i != e; ++i)
    Ret[i] = Args[i];

  FullApplySite NAI;

  switch (AI.getInstruction()->getKind()) {
  case ValueKind::ApplyInst:
    NAI = Builder.createApply(AI.getLoc(), AI.getCallee(),
                                   AI.getSubstCalleeSILType(),
                                   AI.getType(),
                                   AI.getSubstitutions(),
                                   Ret,
                                   cast<ApplyInst>(AI)->isNonThrowing());
    break;
  case ValueKind::TryApplyInst: {
    auto *TryApplyI = cast<TryApplyInst>(AI.getInstruction());
    NAI = Builder.createTryApply(AI.getLoc(), AI.getCallee(),
                                      AI.getSubstCalleeSILType(),
                                      AI.getSubstitutions(),
                                      Ret,
                                      TryApplyI->getNormalBB(),
                                      TryApplyI->getErrorBB());
    }
    break;
  default:
    llvm_unreachable("Trying to clone an unsupported apply instruction");
  }

  NAI.getInstruction();
  return NAI;
}

/// Insert monomorphic inline caches for a specific class or metatype
/// type \p SubClassTy.
static FullApplySite speculateMonomorphicTarget(FullApplySite AI,
                                                SILType SubType,
                                                CheckedCastBranchInst *&CCBI) {
  CCBI = nullptr;
  // Bail if this class_method cannot be devirtualized.
  if (!canDevirtualizeClassMethod(AI, SubType))
    return FullApplySite();

  // Create a diamond shaped control flow and a checked_cast_branch
  // instruction that checks the exact type of the object.
  // This cast selects between two paths: one that calls the slow dynamic
  // dispatch and one that calls the specific method.
  auto It = AI.getInstruction()->getIterator();
  SILFunction *F = AI.getFunction();
  SILBasicBlock *Entry = AI.getParent();

  // Iden is the basic block containing the direct call.
  SILBasicBlock *Iden = F->createBasicBlock();
  // Virt is the block containing the slow virtual call.
  SILBasicBlock *Virt = F->createBasicBlock();
  Iden->createBBArg(SubType);

  SILBasicBlock *Continue = Entry->splitBasicBlock(It);

  SILBuilderWithScope Builder(Entry, AI.getInstruction());
  // Create the checked_cast_branch instruction that checks at runtime if the
  // class instance is identical to the SILType.

  //ClassMethodInst *CMI = cast<ClassMethodInst>(AI.getCallee());
  MethodInst *CMI = cast<MethodInst>(AI.getCallee());

  CCBI = Builder.createCheckedCastBranch(AI.getLoc(), /*exact*/ true,
                                       CMI->getOperand(0), SubType, Iden,
                                       Virt);
  It = CCBI->getIterator();

  SILBuilderWithScope VirtBuilder(Virt, AI.getInstruction());
  SILBuilderWithScope IdenBuilder(Iden, AI.getInstruction());
  // This is the class reference downcasted into subclass SubType.
  SILValue DownCastedClassInstance = Iden->getBBArg(0);

  // Copy the two apply instructions into the two blocks.
  FullApplySite IdenAI = CloneApply(AI, IdenBuilder);
  FullApplySite VirtAI = CloneApply(AI, VirtBuilder);

  // See if Continue has a release on self as the instruction right after the
  // apply. If it exists, move it into position in the diamond.
  if (auto *Release =
          dyn_cast<StrongReleaseInst>(std::next(Continue->begin()))) {
    if (Release->getOperand() == CMI->getOperand(0)) {
      VirtBuilder.createStrongRelease(Release->getLoc(), CMI->getOperand(0),
                                      Atomicity::Atomic);
      IdenBuilder.createStrongRelease(
          Release->getLoc(), DownCastedClassInstance, Atomicity::Atomic);
      Release->eraseFromParent();
    }
  }

  // Create a PHInode for returning the return value from both apply
  // instructions.
  SILArgument *Arg = Continue->createBBArg(AI.getType());
  if (!isa<TryApplyInst>(AI)) {
    IdenBuilder.createBranch(AI.getLoc(), Continue,
                             ArrayRef<SILValue>(IdenAI.getInstruction()));
    VirtBuilder.createBranch(AI.getLoc(), Continue,
                             ArrayRef<SILValue>(VirtAI.getInstruction()));
  }

  // Remove the old Apply instruction.
  assert(AI.getInstruction() == &Continue->front() &&
         "AI should be the first instruction in the split Continue block");
  if (!isa<TryApplyInst>(AI)) {
    AI.getInstruction()->replaceAllUsesWith(Arg);
    AI.getInstruction()->eraseFromParent();
    assert(!Continue->empty() &&
           "There should be at least a terminator after AI");
  } else {
    AI.getInstruction()->eraseFromParent();
    assert(Continue->empty() &&
           "There should not be an instruction after try_apply");
    Continue->eraseFromParent();
  }

  // Update the stats.
  NumTargetsPredicted++;

  // Devirtualize the apply instruction on the identical path.
  auto NewInstPair = devirtualizeClassMethod(IdenAI, DownCastedClassInstance);
  assert(NewInstPair.first && "Expected to be able to devirtualize apply!");
  replaceDeadApply(IdenAI, NewInstPair.first);

  // Split critical edges resulting from VirtAI.
  if (auto *TAI = dyn_cast<TryApplyInst>(VirtAI)) {
    auto *ErrorBB = TAI->getFunction()->createBasicBlock();
    ErrorBB->createBBArg(TAI->getErrorBB()->getBBArg(0)->getType());
    Builder.setInsertionPoint(ErrorBB);
    Builder.createBranch(TAI->getLoc(), TAI->getErrorBB(),
                         {ErrorBB->getBBArg(0)});

    auto *NormalBB = TAI->getFunction()->createBasicBlock();
    NormalBB->createBBArg(TAI->getNormalBB()->getBBArg(0)->getType());
    Builder.setInsertionPoint(NormalBB);
    Builder.createBranch(TAI->getLoc(), TAI->getNormalBB(),
                        {NormalBB->getBBArg(0) });

    Builder.setInsertionPoint(VirtAI.getInstruction());
    SmallVector<SILValue, 4> Args;
    for (auto Arg : VirtAI.getArguments()) {
      Args.push_back(Arg);
    }
    FullApplySite NewVirtAI = Builder.createTryApply(VirtAI.getLoc(), VirtAI.getCallee(),
        VirtAI.getSubstCalleeSILType(), VirtAI.getSubstitutions(),
        Args, NormalBB, ErrorBB);
    VirtAI.getInstruction()->eraseFromParent();
    VirtAI = NewVirtAI;
  }

  return VirtAI;
}

static FullApplySite speculateMonomorphicMetatypeTarget(FullApplySite AI,
                                                SILType SubType,
                                                CheckedCastAddrBranchInst *&CCBI) {
  llvm::dbgs() << "\nCannot devirt witness_method invocations on metatypes yet."
    << " In function: " << AI.getFunction()->getName() << "\n";
  AI.getInstruction()->dump();
  return FullApplySite();
}

/// TODO: The following helper functions are borrowed from the @specialize
/// implementation. Make them utility functions.

/// Returns the thick metatype for the given SILType.
/// e.g. $*T -> $@thick T.Type
static SILType getThickMetatypeType(CanType Ty) {
  // If it is a metatype already, simply return it.
  if (isa<MetatypeType>(Ty))
    return SILType::getPrimitiveObjectType(Ty);

  auto SwiftTy = CanMetatypeType::get(Ty, MetatypeRepresentation::Thick);
  return SILType::getPrimitiveObjectType(SwiftTy);
}

// Emits a type check in the current block.
// Advances the builder to the successful type check's block.
// 
// Precondition: Builder's current insertion block is not terminated.
//
// Postcondition: Builder's insertion block is a new block that defines the
// specialized call argument and has not been terminated.
//
// The type check is emitted in the current block as: 
// metatype $@thick T.Type
// %a = unchecked_bitwise_cast % to $Builtin.Int64
// metatype $@thick <Specialized>.Type
// %b = unchecked_bitwise_cast % to $Builtin.Int64
// builtin "cmp_eq_Int64"(%a : $Builtin.Int64, %b : $Builtin.Int64)
//   : $Builtin.Int1
// cond_br %
static void
emitTypeCheck1(SILBuilder &Builder, SILLocation &Loc,
              SILBasicBlock *FailedTypeCheckBB, SILType SelfTy,
              SILType SubTy) {
  // Instantiate a thick metatype for T.Type
  auto GenericMT = Builder.createMetatype(
    Loc, getThickMetatypeType(SelfTy.getSwiftRValueType()));

  // Instantiate a thick metatype for <Specialized>.Type
  auto SpecializedMT = Builder.createMetatype(
    Loc, getThickMetatypeType(SubTy.getSwiftRValueType()));

  auto &Ctx = Builder.getASTContext();
  auto WordTy = SILType::getBuiltinWordType(Ctx);
  auto GenericMTVal =
    Builder.createUncheckedBitwiseCast(Loc, GenericMT, WordTy);
  auto SpecializedMTVal =
    Builder.createUncheckedBitwiseCast(Loc, SpecializedMT, WordTy);

  auto Cmp =
    Builder.createBuiltinBinaryFunction(Loc, "cmp_eq", WordTy,
                                        SILType::getBuiltinIntegerType(1, Ctx),
                                        {GenericMTVal, SpecializedMTVal});

  auto *SuccessBB = Builder.getFunction().createBasicBlock();
  Builder.createCondBranch(Loc, Cmp, SuccessBB, FailedTypeCheckBB);
  Builder.emitBlock(SuccessBB);
}

static void
emitTypeCheck(SILBuilder &Builder, SILLocation &Loc,
              SILBasicBlock *FailedTypeCheckBB, SILType SelfTy,
              SILType SubTy) {
  auto Cmp = Builder.createIsSameTypeInst(Loc, SelfTy.getSwiftRValueType(),
                                          SubTy.getSwiftRValueType());
  auto *SuccessBB = Builder.getFunction().createBasicBlock();
  Builder.createCondBranch(Loc, Cmp, SuccessBB, FailedTypeCheckBB);
  Builder.emitBlock(SuccessBB);
}

// Insert monomorphic inline caches for a specific class or metatype
/// type \p SubClassTy.
static FullApplySite speculateMonomorphicTarget(FullApplySite AI,
                                                SILType SubType,
                                                SILValue SelfCopyAlloc,
                                                CheckedCastAddrBranchInst *&CCBI,
                                                SmallVectorImpl<SILInstruction *> &InsertedDeallocs
) {
  CCBI = nullptr;

  MethodInst *CMI = cast<MethodInst>(AI.getCallee());
  assert(AI.hasSelfArgument());
  auto Self = AI.getSelfArgument();
  bool isMetatype = false;
  // Is it a call on a metatype? E.g. a constructor call or static/class method call?
  //if (isa<MetatypeType>(Self->getType().getSwiftRValueType())) {
  if (Self->getType().is<MetatypeType>()) {
    //return speculateMonomorphicMetatypeTarget(AI, SubType, CCBI);
    isMetatype = true;
  }

  // Bail if this class_method cannot be devirtualized.
  SILType LookupType = SubType;
  if (isMetatype && SubType.is<MetatypeType>()) {
    //assert(SubType.is<MetatypeType>() && "Sub-type should be a metatype");
    LookupType = SubType.getMetatypeInstanceType(AI.getModule());
  }

  if (!canDevirtualizeClassMethod(AI, LookupType))
    return FullApplySite();


  // TODO: If self is open_existential_addr, then the result
  // of open_existential_addr is the address of the payload.
  // It should be used as a self for the devirtualized method.
  // this may require casting into a proper pointer type required
  // by the devirtualized method.
  // In general, Self should be used as self for a devirtualized method.
  auto *OEAI = dyn_cast<OpenExistentialAddrInst>(Self);

  // Find the original exsistential which is referred by the opened_existential used by
  // the apply instruciton.
  SILValue OrigExistential;

  // Handle the case where Self is a "metatype $@thick U.Type" instruction.
  // It happens e.g. if one calls an initializer on this value.
  if (OEAI)
    OrigExistential = OEAI->getOperand();
  else
    OrigExistential = Self;

  /*
  if (!OEAI) {
    llvm::dbgs() << "\nCannot speculateMonomorphicTarget in function "
                 << AI.getFunction()->getName() << ":\n";
    AI.getInstruction()->dumpInContext();
    return FullApplySite();
  }
  */

  // Create a diamond shaped control flow and a checked_cast_addr_br
  // instruction that checks the exact type of the object.
  // This cast selects between two paths: one that calls the slow dynamic
  // dispatch and one that calls the specific method.
  auto It = AI.getInstruction()->getIterator();
  SILFunction *F = AI.getFunction();
  SILBasicBlock *Entry = AI.getParent();

  // Iden is the basic block containing the direct call.
  SILBasicBlock *Iden = F->createBasicBlock();
  // Virt is the block containing the slow virtual call.
  SILBasicBlock *Virt = F->createBasicBlock();
  //Iden->createBBArg(SubType);

  SILBasicBlock *Continue = Entry->splitBasicBlock(It);

  SILBuilderWithScope Builder(Entry, AI.getInstruction());

#if 0
  SILValue SelfCopyAlloc;

  // TODO: It is probably enough to allocate the copy only once for
  // any given switch with multiple alternatives.
  SelfCopyAlloc =
      Builder.createAllocStack(AI.getLoc(), OrigExistential->getType());
  Builder.createCopyAddr(AI.getLoc(), OrigExistential, SelfCopyAlloc,
                         /* isTake */ IsTake_t::IsNotTake,
                         /* isInit */ IsInitialization_t::IsInitialization);
#endif

  auto Loc = AI.getLoc();
  ////SILValue TargetAlloc;
  // create alloc_stack for a target type.
  ////TargetAlloc = Builder.createAllocStack(AI.getLoc(), SubType);

  // Check if the type of of self is identical to the subtype.
  // To do this, extract the information about the static type from the
  // existential or extract the information from the argument of a generic type.
  //
  // Create the checked_cast_addr_branch instruction that checks at runtime if the
  // class instance is identical to the SILType.
  //
  if (isMetatype) {
    emitTypeCheck(Builder, Loc, Virt,
                  Self->getType().getMetatypeInstanceType(AI.getModule()),
                  SubType);
    Builder.createBranch(Loc, Iden);
  } else if (!OEAI) {
    // Self is not an existential. Switch on its metatype.
    emitTypeCheck(Builder, Loc, Virt, Self->getType().getObjectType(), SubType);
    Builder.createBranch(Loc, Iden);
  } else {
    emitTypeCheck(Builder, Loc, Virt, Self->getType().getObjectType(), SubType);
    Builder.createBranch(Loc, Iden);
#if 0
    CCBI = Builder.createCheckedCastAddrBranch(
      AI.getLoc(), /*exact*/ true, CastConsumptionKind::TakeOnSuccess,
      //AI.getLoc(), /*exact*/ true, CastConsumptionKind::TakeAlways,
      SelfCopyAlloc, SelfCopyAlloc->getType().getSwiftRValueType(),
      TargetAlloc, TargetAlloc->getType().getSwiftRValueType(),
      Iden, Virt);

  It = CCBI->getIterator();
#endif
  }

  SILBuilderWithScope VirtBuilder(Virt, AI.getInstruction());
  SILBuilderWithScope IdenBuilder(Iden, AI.getInstruction());

  ////InsertedDeallocs.push_back(VirtBuilder.createDeallocStack(Loc, TargetAlloc));
  //VirtBuilder.createDeallocStack(Loc, SelfCopyAlloc);

  //if (!OrigExistential->getType().isTrivial(AI.getModule()))
  //  IdenBuilder.createDestroyAddr(AI.getLoc(), OrigExistential);
  // IdenBuilder.createDestroyAddr(AI.getLoc(), SelfCopyAlloc);

  // Load the result of the cast.
  //SILValue DownCastedClassInstance = IdenBuilder.createLoad(AI.getLoc(), TargetAlloc);
  SILValue DownCastedClassInstance;
  // TODO: It could happen that this copy is used later. Thus, TargetAlloc cannot
  // be deallocated yet.
//  DownCastedClassInstance = TargetAlloc;

  // Self is always the address of the actual payload inside the existential or
  // the address of the argument of a generic type conforming to a protocol.
  DownCastedClassInstance = Self;
  if (!isMetatype) {
  // Cast self to type required by the devirtualized function.
  DownCastedClassInstance =
      castValueToABICompatibleType(&IdenBuilder, AI.getLoc(), Self,
                                   Self->getType(), SubType.getAddressType())
          .getValue();
  } else {
    DownCastedClassInstance = IdenBuilder.createMetatype(
        AI.getLoc(), getThickMetatypeType(SubType.getSwiftRValueType()));
  }
  //  IdenBuilder.createDeallocStack(Loc, TargetAlloc);
  //  IdenBuilder.createDeallocStack(Loc, SelfCopyAlloc);

  // Copy the two apply instructions into the two blocks.
  FullApplySite IdenAI = CloneApply(AI, IdenBuilder);
  FullApplySite VirtAI = CloneApply(AI, VirtBuilder);

  // See if Continue has a release on self as the instruction right after the
  // apply. If it exists, move it into position in the diamond.
  if (auto *Release =
          dyn_cast<StrongReleaseInst>(std::next(Continue->begin()))) {
    if (Release->getOperand() == Self) {
      VirtBuilder.createStrongRelease(Release->getLoc(), CMI->getOperand(0),
                                      Atomicity::Atomic);
      IdenBuilder.createStrongRelease(
          Release->getLoc(), DownCastedClassInstance, Atomicity::Atomic);
      Release->eraseFromParent();
    }
  }

  // Create a PHInode for returning the return value from both apply
  // instructions.
  SILArgument *Arg = Continue->createBBArg(AI.getType());
  if (!isa<TryApplyInst>(AI)) {
    IdenBuilder.createBranch(AI.getLoc(), Continue,
                             ArrayRef<SILValue>(IdenAI.getInstruction()));
    VirtBuilder.createBranch(AI.getLoc(), Continue,
                             ArrayRef<SILValue>(VirtAI.getInstruction()));
  }

  // Remove the old Apply instruction.
  assert(AI.getInstruction() == &Continue->front() &&
         "AI should be the first instruction in the split Continue block");
  if (!isa<TryApplyInst>(AI)) {
    AI.getInstruction()->replaceAllUsesWith(Arg);
    AI.getInstruction()->eraseFromParent();
    assert(!Continue->empty() &&
           "There should be at least a terminator after AI");
  } else {
    AI.getInstruction()->eraseFromParent();
    assert(Continue->empty() &&
           "There should not be an instruction after try_apply");
    Continue->eraseFromParent();
  }

  // Update the stats.
  NumTargetsPredicted++;

  // Devirtualize the apply instruction on the identical path.
  // TODO: If we are performing a devirtualized call on a copy of self,
  // the call may mutate this copy. But it will not mutate the original
  // existential self. Therefore, we need to write the mutated copy
  // back to the existential self after the call to mimic the original
  // semantics.
  auto NewInstPair = devirtualizeClassMethod(IdenAI, DownCastedClassInstance);
  assert(NewInstPair.first && "Expected to be able to devirtualize apply!");
  replaceDeadApply(IdenAI, NewInstPair.first);

  // It is safe to dealloc values after the call.
  if (NewInstPair.first->getParentBB()->getTerminator() != NewInstPair.first) {
    SILBuilderWithScope Builder(
        &*std::next(NewInstPair.second.getInstruction()->getIterator()),
        NewInstPair.second.getInstruction());
    ////InsertedDeallocs.push_back(Builder.createDeallocStack(Loc, TargetAlloc));
    //Builder.createDeallocStack(Loc, SelfCopyAlloc);
  } else {
    // Insert the dealloc at the beginning of all successor blocks.
    for (auto SuccBB : NewInstPair.first->getParentBB()->getSuccessorBlocks()) {
      assert(SuccBB->getSinglePredecessor() &&
             "Successor blocks should have a single predecessor");
      ////SILBuilderWithScope Builder(&SuccBB->front());
      ////InsertedDeallocs.push_back(Builder.createDeallocStack(Loc, TargetAlloc));
    }
    //llvm_unreachable("try_apply not supported yet");
  }

  // Split critical edges resulting from VirtAI.
  if (auto *TAI = dyn_cast<TryApplyInst>(VirtAI)) {
    auto *ErrorBB = TAI->getFunction()->createBasicBlock();
    ErrorBB->createBBArg(TAI->getErrorBB()->getBBArg(0)->getType());
    Builder.setInsertionPoint(ErrorBB);
    Builder.createBranch(TAI->getLoc(), TAI->getErrorBB(),
                         {ErrorBB->getBBArg(0)});

    auto *NormalBB = TAI->getFunction()->createBasicBlock();
    NormalBB->createBBArg(TAI->getNormalBB()->getBBArg(0)->getType());
    Builder.setInsertionPoint(NormalBB);
    Builder.createBranch(TAI->getLoc(), TAI->getNormalBB(),
                        {NormalBB->getBBArg(0) });

    Builder.setInsertionPoint(VirtAI.getInstruction());
    SmallVector<SILValue, 4> Args;
    for (auto Arg : VirtAI.getArguments()) {
      Args.push_back(Arg);
    }
    FullApplySite NewVirtAI = Builder.createTryApply(VirtAI.getLoc(), VirtAI.getCallee(),
        VirtAI.getSubstCalleeSILType(), VirtAI.getSubstitutions(),
        Args, NormalBB, ErrorBB);
    VirtAI.getInstruction()->eraseFromParent();
    VirtAI = NewVirtAI;
  }

  return VirtAI;
}

/// \brief Returns true, if a method implementation to be called by the
/// default case handler of a speculative devirtualization is statically
/// known. This happens if it can be proven that generated
/// checked_cast_br instructions cover all other possible cases.
///
/// \p CHA class hierarchy analysis to be used
/// \p AI  invocation instruction
/// \p CD  static class of the instance whose method is being invoked
/// \p Subs set of direct subclasses of this class
static bool isDefaultCaseKnown(ClassHierarchyAnalysis *CHA,
                               FullApplySite AI,
                               ClassDecl *CD,
                               ClassHierarchyAnalysis::ClassList &Subs) {
  ClassMethodInst *CMI = cast<ClassMethodInst>(AI.getCallee());
  auto *Method = CMI->getMember().getFuncDecl();
  const DeclContext *DC = AI.getModule().getAssociatedContext();

  if (CD->isFinal())
    return true;

  // If the class has an @objc ancestry it can be dynamically subclassed and we
  // can't therefore statically know the default case.
  auto Ancestry = CD->checkObjCAncestry();
  if (Ancestry != ObjCClassKind::NonObjC)
    return false;

  // Without an associated context we cannot perform any
  // access-based optimizations.
  if (!DC)
    return false;

  // Only handle classes defined within the SILModule's associated context.
  if (!CD->isChildContextOf(DC))
    return false;

  if (!CD->hasAccessibility())
    return false;

  // Only consider 'private' members, unless we are in whole-module compilation.
  switch (CD->getEffectiveAccess()) {
  case Accessibility::Public:
    return false;
  case Accessibility::Internal:
    if (!AI.getModule().isWholeModule())
      return false;
    break;
  case Accessibility::Private:
    break;
  }

  // This is a private or a module internal class.
  //
  // We can analyze the class hierarchy rooted at it and
  // eventually devirtualize a method call more efficiently.

  // First, analyze all direct subclasses.
  // We know that a dedicated checked_cast_br check is
  // generated for each direct subclass by tryToSpeculateTarget.
  for (auto S : Subs) {
    // Check if the subclass overrides a method
    auto *FD = S->findOverridingDecl(Method);
    if (!FD)
      continue;
    if (CHA->hasKnownDirectSubclasses(S)) {
      // This subclass has its own subclasses and
      // they will use this implementation or provide
      // their own. In either case it is not covered by
      // checked_cast_br instructions generated by
      // tryToSpeculateTarget. Therefore it increases
      // the number of remaining cases to be handled
      // by the default case handler.
      return false;
    }
  }

  // Then, analyze indirect subclasses.

  // Set of indirect subclasses for the class.
  auto &IndirectSubs = CHA->getIndirectSubClasses(CD);

  // Check if any indirect subclasses use an implementation
  // of the method different from the implementation in
  // the current class. If this is the case, then such
  // an indirect subclass would need a dedicated
  // checked_cast_br check to be devirtualized. But this is
  // not done by tryToSpeculateTarget yet and therefore
  // such a subclass should be handled by the "default"
  // case handler, which essentially means that "default"
  // case cannot be devirtualized since it covers more
  // then one alternative.
  for (auto S : IndirectSubs) {
    auto *ImplFD = S->findImplementingMethod(Method);
    if (ImplFD != Method) {
      // Different implementation is used by a subclass.
      // Therefore, the default case is not known.
      return false;
    }
  }

  return true;
}

/// \brief Try to speculate the call target for the call \p AI. This function
/// returns true if a change was made.
static bool tryToSpeculateTarget(FullApplySite AI,
                                 ClassHierarchyAnalysis *CHA,
                                 ClassMethodInst *CMI) {
  // We cannot devirtualize in cases where dynamic calls are
  // semantically required.
  if (CMI->isVolatile())
    return false;

  // Strip any upcasts off of our 'self' value, potentially leaving us
  // with a value whose type is closer (in the class hierarchy) to the
  // actual dynamic type.
  auto SubTypeValue = stripUpCasts(CMI->getOperand());
  SILType SubType = SubTypeValue->getType();

  // Bail if any generic types parameters of the class instance type are
  // unbound.
  // We cannot devirtualize unbound generic calls yet.
  if (isNominalTypeWithUnboundGenericParameters(SubType, AI.getModule()))
    return false;

  auto &M = CMI->getModule();
  auto ClassType = SubType;
  if (SubType.is<MetatypeType>())
    ClassType = SubType.getMetatypeInstanceType(M);

  CheckedCastBranchInst *LastCCBI = nullptr;

  ClassDecl *CD = ClassType.getClassOrBoundGenericClass();
  assert(CD && "Expected decl for class type!");

  if (!CHA->hasKnownDirectSubclasses(CD)) {
    // If there is only one possible alternative for this method,
    // try to devirtualize it completely.
    ClassHierarchyAnalysis::ClassList Subs;
    if (isDefaultCaseKnown(CHA, AI, CD, Subs)) {
      auto NewInstPair = tryDevirtualizeClassMethod(AI, SubTypeValue);
      if (NewInstPair.first)
        replaceDeadApply(AI, NewInstPair.first);
      return NewInstPair.second.getInstruction() != nullptr;
    }

    DEBUG(llvm::dbgs() << "Inserting monomorphic speculative call for class " <<
          CD->getName() << "\n");
    return !!speculateMonomorphicTarget(AI, SubType, LastCCBI);
  }

  // True if any instructions were changed or generated.
  bool Changed = false;

  // Collect the direct and indirect subclasses for the class.
  // Sort these subclasses in the order they should be tested by the
  // speculative devirtualization. Different strategies could be used,
  // E.g. breadth-first, depth-first, etc.
  // Currently, let's use the breadth-first strategy.
  // The exact static type of the instance should be tested first.
  auto &DirectSubs = CHA->getDirectSubClasses(CD);
  auto &IndirectSubs = CHA->getIndirectSubClasses(CD);

  SmallVector<ClassDecl *, 8> Subs(DirectSubs);
  Subs.append(IndirectSubs.begin(), IndirectSubs.end());

  if (isa<BoundGenericClassType>(ClassType.getSwiftRValueType())) {
    // Filter out any subclasses that do not inherit from this
    // specific bound class.
    auto RemovedIt = std::remove_if(Subs.begin(),
        Subs.end(),
        [&ClassType, &M](ClassDecl *Sub){
          auto SubCanTy = Sub->getDeclaredType()->getCanonicalType();
          // Unbound generic type can override a method from
          // a bound generic class, but this unbound generic
          // class is not considered to be a subclass of a
          // bound generic class in a general case.
          if (isa<UnboundGenericType>(SubCanTy))
            return false;
          // Handle the usual case here: the class in question
          // should be a real subclass of a bound generic class.
          return !ClassType.isBindableToSuperclassOf(
              SILType::getPrimitiveObjectType(SubCanTy));
        });
    Subs.erase(RemovedIt, Subs.end());
  }

  // Number of subclasses which cannot be handled by checked_cast_br checks.
  int NotHandledSubsNum = 0;
  if (Subs.size() > MaxNumSpeculativeTargets) {
    DEBUG(llvm::dbgs() << "Class " << CD->getName() << " has too many ("
                       << Subs.size() << ") subclasses. Performing speculative "
                         "devirtualization only for the first "
                       << MaxNumSpeculativeTargets << " of them.\n");

    NotHandledSubsNum += (Subs.size() - MaxNumSpeculativeTargets);
    Subs.erase(&Subs[MaxNumSpeculativeTargets], Subs.end());
  }

  DEBUG(llvm::dbgs() << "Class " << CD->getName() << " is a superclass. "
        "Inserting polymorphic speculative call.\n");

  // Try to devirtualize the static class of instance
  // if it is possible.
  auto FirstAI = speculateMonomorphicTarget(AI, SubType, LastCCBI);
  if (FirstAI) {
    Changed = true;
    AI = FirstAI;
  }

  // Perform a speculative devirtualization of a method invocation.
  // It replaces an indirect class_method-based call by a code to perform
  // a direct call of the method implementation based on the dynamic class
  // of the instance.
  //
  // The code is generated according to the following principles:
  //
  // - For each direct subclass, a dedicated checked_cast_br instruction
  // is generated to check if a dynamic class of the instance is exactly
  // this subclass.
  //
  // - If this check succeeds, then it jumps to the code which performs a
  // direct call of a method implementation specific to this subclass.
  //
  // - If this check fails, then a different subclass is checked by means of
  // checked_cast_br in a similar way.
  //
  // - Finally, if the instance does not exactly match any of the direct
  // subclasses, the "default" case code is generated, which should handle
  // all remaining alternatives, i.e. it should be able to dispatch to any
  // possible remaining method implementations. Typically this is achieved by
  // using a class_method instruction, which performs an indirect invocation.
  // But if it can be proven that only one specific implementation of
  // a method will be always invoked by this code, then a class_method-based
  // call can be devirtualized and replaced by a more efficient direct
  // invocation of this specific method implementation.
  //
  // Remark: With the current implementation of a speculative devirtualization,
  // if devirtualization of the "default" case is possible, then it would
  // by construction directly invoke the implementation of the method
  // corresponding to the static type of the instance. This may change
  // in the future, if we start using PGO for ordering of checked_cast_br
  // checks.

  // TODO: The ordering of checks may benefit from using a PGO, because
  // the most probable alternatives could be checked first.

  for (auto S : Subs) {
    DEBUG(llvm::dbgs() << "Inserting a speculative call for class "
          << CD->getName() << " and subclass " << S->getName() << "\n");

    CanType CanClassType = S->getDeclaredType()->getCanonicalType();
    SILType ClassType = SILType::getPrimitiveObjectType(CanClassType);
    if (!ClassType.getClassOrBoundGenericClass()) {
      // This subclass cannot be handled. This happens e.g. if it is
      // a generic class.
      NotHandledSubsNum++;
      continue;
    }

    auto ClassOrMetatypeType = ClassType;
    if (auto EMT = SubType.getAs<AnyMetatypeType>()) {
      auto InstTy = ClassType.getSwiftRValueType();
      auto *MetaTy = MetatypeType::get(InstTy, EMT->getRepresentation());
      auto CanMetaTy = CanMetatypeType::CanTypeWrapper(MetaTy);
      ClassOrMetatypeType = SILType::getPrimitiveObjectType(CanMetaTy);
    }

    // Pass the metatype of the subclass.
    auto NewAI = speculateMonomorphicTarget(AI, ClassOrMetatypeType, LastCCBI);
    if (!NewAI) {
      NotHandledSubsNum++;
      continue;
    }
    AI = NewAI;
    Changed = true;
  }

  // Check if there is only a single statically known implementation
  // of the method which can be called by the default case handler.
  if (NotHandledSubsNum || !isDefaultCaseKnown(CHA, AI, CD, Subs)) {
    // Devirtualization of remaining cases is not possible,
    // because more than one implementation of the method
    // needs to be handled here. Thus, an indirect call through
    // the class_method cannot be eliminated completely.
    //
    return Changed;
  }

  // At this point it is known that there is only one remaining method
  // implementation which is not covered by checked_cast_br checks yet.
  // So, it is safe to replace a class_method invocation by
  // a direct call of this remaining implementation.
  if (LastCCBI && SubTypeValue == LastCCBI->getOperand()) {
    // Remove last checked_cast_br, because it will always succeed.
    SILBuilderWithScope B(LastCCBI);
    auto CastedValue = B.createUncheckedBitCast(LastCCBI->getLoc(),
                                                LastCCBI->getOperand(),
                                                LastCCBI->getCastType());
    B.createBranch(LastCCBI->getLoc(), LastCCBI->getSuccessBB(), {CastedValue});
    LastCCBI->eraseFromParent();
    return true;
  }
  auto NewInstPair = tryDevirtualizeClassMethod(AI, SubTypeValue);
  if (NewInstPair.first) {
    replaceDeadApply(AI, NewInstPair.first);
    return true;
  }
  return Changed;
}

/// \brief Try to speculate the call target for the call \p AI. This function
/// returns true if a change was made.
static bool tryToSpeculateTarget(FullApplySite AI,
                                 ClassHierarchyAnalysis *CHA,
                                 WitnessMethodInst *CMI) {
  // We cannot devirtualize in cases where dynamic calls are
  // semantically required.
  if (CMI->isVolatile())
    return false;

  ProtocolDecl *WMIProtocol = CMI->getLookupProtocol();

  if (!SpecDevirtNonClassProtocols) {
    // Support only devirtualization of class protocols.
    if (!WMIProtocol->requiresClass())
      return false;
  }

  if (!WMIProtocol->requiresClass()) {
    if (!shouldProcessFunction(Functions, AI.getFunction()->getName(), startIdx, endIdx))
      return false;
  }

  // Strip any upcasts off of our 'self' value, potentially leaving us
  // with a value whose type is closer (in the class hierarchy) to the
  // actual dynamic type.
  // CMI->getOperand() crashes, if this is a call like in
  // func foo<T:S> (t: T) -> Int32 { return t.foo() }
  SILType SubType;
  if (CMI->hasOperand()) {
    SubType = CMI->getOperand()->getType();
  } else {
    SubType = SILType::getPrimitiveObjectType(CMI->getLookupType());
  }

  auto &M = CMI->getModule();
  auto ClassType = SubType;
  // In case of metatypes, switch on the instance type.
  if (SubType.is<MetatypeType>())
    ClassType = SubType.getMetatypeInstanceType(M);

  if (!CHA->hasKnownImplementations(WMIProtocol)) {
    return false;
  }

  auto &Impls = CHA->getProtocolImplementations(WMIProtocol);

  SmallVector<NominalTypeDecl *, 8> Subs(Impls);

  if (isa<BoundGenericClassType>(ClassType.getSwiftRValueType())) {
    // Filter out any subclasses that do not inherit from this
    // specific bound class.
    auto RemovedIt = std::remove_if(Subs.begin(),
        Subs.end(),
        [&ClassType, &M](NominalTypeDecl *Sub){
          auto SubCanTy = Sub->getDeclaredType()->getCanonicalType();
          // Unbound generic type can override a method from
          // a bound generic class, but this unbound generic
          // class is not considered to be a subclass of a
          // bound generic class in a general case.
          if (isa<UnboundGenericType>(SubCanTy))
            return false;
          // Handle the usual case here: the class in question
          // should be a real subclass of a bound generic class.
          return !ClassType.isBindableToSuperclassOf(
              SILType::getPrimitiveObjectType(SubCanTy));
        });
    Subs.erase(RemovedIt, Subs.end());
  }

  if (Subs.size() > MaxNumSpeculativeTargets) {
    // TODO: Use PGO to handle the most probable alternatives.
    DEBUG(llvm::dbgs() << "Protocol " << WMIProtocol->getName() << " has too many (" <<
          Subs.size() << ") implementations. Not speculating.\n");
    return false;
  }

  DEBUG(llvm::dbgs() << "Protocol " << WMIProtocol->getName()
                     << " has multiple known implementations. "
                        "Inserting polymorphic speculative call.\n");

  // Perform a speculative devirtualization of a method invocation.
  // It replaces an indirect witness_method-based call by a code to perform
  // a direct call of the method implementation based on the dynamic class
  // of the instance.
  //
  // The code is generated according to the following principles:
  //
  // - For each class implementing a protocol, a dedicated checked_cast_br instruction
  // is generated to check if a dynamic class of the instance is exactly
  // this class.
  //
  // - If this check succeeds, then it jumps to the code which performs a
  // direct call of a method implementation specific to this class.
  //
  // - If this check fails, then a different class is checked by means of
  // checked_cast_br in a similar way.
  //
  // TODO: The ordering of checks may benefit from using a PGO, because
  // the most probable alternatives could be checked first.

  SILInstruction *SelfCopyAlloc = nullptr;
  SILInstruction *CopyAddrSelf = nullptr;
  auto Loc = AI.getLoc();
  SmallVector<SILInstruction *, 8> InsertedDeallocs;
  if (!WMIProtocol->requiresClass()) {
    // Create a copy of Self.
    // It is enough to allocate the copy only once for
    // any given switch with multiple alternatives.
    auto Self = AI.getSelfArgument();
    auto *OEAI = dyn_cast<OpenExistentialAddrInst>(Self);

    // Find the original exsistential which is referred by the
    // opened_existential used by the apply instruciton.
    SILValue OrigExistential;

    // Handle the case where Self is a "metatype $@thick U.Type" instruction.
    // It happens e.g. if one calls an initializer on this value.
    if (OEAI)
      OrigExistential = OEAI->getOperand();
    else
      OrigExistential = Self;

    if (!isa<MetatypeType>(Self->getType().getSwiftRValueType())) {

      SILBuilderWithScope Builder(AI.getInstruction());
      SelfCopyAlloc =
          Builder.createAllocStack(AI.getLoc(), OrigExistential->getType());
      CopyAddrSelf = Builder.createCopyAddr(AI.getLoc(), OrigExistential, SelfCopyAlloc,
                             /* isTake */ IsTake_t::IsNotTake,
                             /* isInit */ IsInitialization_t::IsInitialization);
#if 0
      // Insert dealloc of SelfCopyAlloc if needed.
      if (auto *TAI = dyn_cast<TryApplyInst>(AI.getInstruction())) {
        SILBuilderWithScope BuilderNormal(&TAI->getNormalBB()->front());
        SILBuilderWithScope BuilderError(&TAI->getErrorBB()->front());
        BuilderNormal.createDeallocStack(Loc, SelfCopyAlloc);
        BuilderError.createDeallocStack(Loc, SelfCopyAlloc);
      } else {
        SILBuilderWithScope BuilderDealloc(
            &*std::next(AI.getInstruction()->getIterator()));
        BuilderDealloc.createDeallocStack(Loc, SelfCopyAlloc);
      }
#endif
    }
  }

  // Number of subclasses which cannot be handled by checked_cast_br checks.
  int NotHandledSubsNum = 0;
  // True if any instructions were changed or generated.
  bool Changed = false;

  for (auto S : Subs) {
    DEBUG(llvm::dbgs() << "Inserting a speculative call for class "
          << WMIProtocol->getName() << " and implementation " << S->getName() << "\n");

    CanType CanTy = S->getDeclaredType()->getCanonicalType();
    if (!CanTy) {
      NotHandledSubsNum++;
      continue;
    }

    SILType ObjectTy = SILType::getPrimitiveObjectType(CanTy);

    if (!ObjectTy.getClassOrBoundGenericClass() &&
        !ObjectTy.getStructOrBoundGenericStruct()) {
      // This subclass cannot be handled. This happens e.g. if it is
      // a generic class.
      //llvm::dbgs() << "Cannot speculatively devirtualize type: "
      //             << (int)ObjectTy.getSwiftRValueType()->getKind() << "\n";
      //CanTy.dump();
      NotHandledSubsNum++;
      continue;
    }

#if 0
    if (false && ObjectTy.getClassOrBoundGenericClass())
    {
      // Do not handle structs yet.
      NotHandledSubsNum++;
      continue;
    }
    if (false && !ObjectTy.isTrivial(AI.getModule()))
    {
      llvm::dbgs() << "Skip non-trivial type: ";
      ObjectTy.dump();

      // Do not handle structs yet.
      NotHandledSubsNum++;
      continue;
    }
#endif
    //llvm::dbgs() << "Speculatively devirt type: ";
    //ObjectTy.dump();


    auto ObjectOrMetatypeType = ObjectTy;
    if (auto EMT = SubType.getAs<AnyMetatypeType>()) {
      auto InstTy = ObjectTy.getSwiftRValueType();
      auto *MetaTy = MetatypeType::get(InstTy, EMT->getRepresentation());
      auto CanMetaTy = CanMetatypeType::CanTypeWrapper(MetaTy);
      ObjectOrMetatypeType = SILType::getPrimitiveObjectType(CanMetaTy);
    }

    FullApplySite NewAI;
    if (WMIProtocol->requiresClass() && !SubType.getAs<AnyMetatypeType>()) {
      // checked_cast_br can be used only for class protocols, because
      // the size of the value needs to be known.
      CheckedCastBranchInst *LastCCBI = nullptr;
      NewAI = speculateMonomorphicTarget(AI, ObjectOrMetatypeType, LastCCBI);
    } else {
      // Pass the metatype of the subclass.
      CheckedCastAddrBranchInst *LastCCABI = nullptr;
      // TODO: Don't use checked_cast_br. Use something else if it is not
      // known that all implementations are classes, i.e. it is effectively
      // a class existential. Most likely, a checked_cast_addr_br [exact] should
      // be used.
      // Collect all inserted dealloc_stacks, because this info is needed to insert
      // a dealloc_stack for the SelfCopyAlloc.
      NewAI = speculateMonomorphicTarget(AI, ObjectOrMetatypeType,
                                         SelfCopyAlloc, LastCCABI,
                                         InsertedDeallocs);
    }
    if (!NewAI) {
      NotHandledSubsNum++;
      continue;
    }
    AI = NewAI;
    Changed = true;
  }

  // Perform clean-up.
  // Add dealloc_stack to match the alloc_stack if this stack location was used.
  // If it was not used, remove the stack_alloc inserted above.
  if (Changed && SelfCopyAlloc) {
    SmallVector<SILInstruction *, 8> Users(InsertedDeallocs);
    for (Operand *Op : SelfCopyAlloc->getUses()) {
      Users.push_back(Op->getUser());
    }
    ValueLifetimeAnalysis VLA(SelfCopyAlloc, Users);
    ValueLifetimeAnalysis::Frontier PAFrontier;
    VLA.computeFrontier(PAFrontier, ValueLifetimeAnalysis::AllowToModifyCFG);
    if (!PAFrontier.empty()) {
      for (auto *EndPoint : PAFrontier) {
        SILBuilderWithScope Builder(EndPoint);
        Builder.createDestroyAddr(Loc, SelfCopyAlloc);
        Builder.createDeallocStack(Loc, SelfCopyAlloc);
      }
    }
  }

  if (!Changed && SelfCopyAlloc) {
    CopyAddrSelf->eraseFromParent();
    SelfCopyAlloc->eraseFromParent();
  }

  return Changed;
}

static bool tryToSpeculateTarget(FullApplySite AI,
                                 ClassHierarchyAnalysis *CHA) {
  if (auto *CMI = dyn_cast<ClassMethodInst>(AI.getCallee()))
      return tryToSpeculateTarget(AI, CHA, CMI);
  if (auto *WMI = dyn_cast<WitnessMethodInst>(AI.getCallee())) {
      return tryToSpeculateTarget(AI, CHA, WMI);
  }
  return false;
}

namespace {
/// Speculate the targets of virtual calls by assuming that the requested
/// class is at the bottom of the class hierarchy.
class SpeculativeDevirtualization : public SILFunctionTransform {
public:
  virtual ~SpeculativeDevirtualization() {}

  void run() override {
    ClassHierarchyAnalysis *CHA = PM->getAnalysis<ClassHierarchyAnalysis>();

    bool Changed = false;

    // Collect virtual calls that may be specialized.
    SmallVector<FullApplySite, 16> ToSpecialize;
    for (auto &BB : *getFunction()) {
      for (auto II = BB.begin(), IE = BB.end(); II != IE; ++II) {
        FullApplySite AI = FullApplySite::isa(&*II);
        if (!AI)
          continue;
        // if (AI.getSubstCalleeType()->getRepresentation() ==
        //    SILFunctionType::Representation::ObjCMethod)
        //  continue;
        if (AI && isa<ClassMethodInst>(AI.getCallee())) {
          ToSpecialize.push_back(AI);
          continue;
        }
        if (AI && isa<WitnessMethodInst>(AI.getCallee())) {
          ToSpecialize.push_back(AI);
          continue;
        }
      }
    }

    // Go over the collected calls and try to insert speculative calls.
    for (auto AI : ToSpecialize)
      Changed |= tryToSpeculateTarget(AI, CHA);

    if (Changed) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::FunctionBody);
    }
  }

  StringRef getName() override { return "Speculative Devirtualization"; }
  };

} // end anonymous namespace

SILTransform *swift::createSpeculativeDevirtualization() {
  return new SpeculativeDevirtualization();
}
