// REQUIRES: tsan_runtime
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -sanitize=thread -swift-version 5 -emit-silgen -primary-file %S/Inputs/OSLogConstantEvaluable.swift -o %t/OSLogConstantEvaluableTsanTest_silgen.sil
//
// Run the (mandatory) passes on which constant evaluator depends, and run the
// constant evaluator on the SIL produced after the dependent passes are run.
//
// RUN: %target-sil-opt -silgen-cleanup -raw-sil-inst-lowering -allocbox-to-stack -mandatory-inlining -constexpr-limit 3072 -test-constant-evaluable-subset %t/OSLogConstantEvaluableTsanTest_silgen.sil > %t/OSLogConstantEvaluableTsanTest.sil 2> %t/error-output
//
// RUN: %FileCheck %S/Inputs/OSLogConstantEvaluable.swift < %t/error-output
//
// REQUIRES: VENDOR=apple
