// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -profile-generate -swift-version 5 -emit-silgen -primary-file %S/Inputs/OSLogConstantEvaluable.swift -o %t/OSLogConstantEvaluableProfilerTest_silgen.sil
//
// Run the (mandatory) passes on which constant evaluator depends, and run the
// constant evaluator on the SIL produced after the dependent passes are run.
//
// RUN: %target-sil-opt -silgen-cleanup -raw-sil-inst-lowering -allocbox-to-stack -mandatory-inlining -constexpr-limit 3072 -test-constant-evaluable-subset %t/OSLogConstantEvaluableProfilerTest_silgen.sil > %t/OSLogConstantEvaluableProfilerTest.sil 2> %t/error-output
//
// RUN: %FileCheck %S/Inputs/OSLogConstantEvaluable.swift < %t/error-output
//
// REQUIRES: VENDOR=apple
