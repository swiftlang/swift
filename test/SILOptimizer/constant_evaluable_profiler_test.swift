// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -profile-generate -enable-upcoming-feature InferSendableFromCaptures -emit-silgen -primary-file %S/Inputs/constant_evaluable.swift -o %t/constant_evaluable_profiler_test_silgen.sil
//
// Run the (mandatory) passes on which constant evaluator depends, and test the
// constant evaluator on the SIL produced after the dependent passes are run.
//
// RUN: not %target-sil-opt -silgen-cleanup -raw-sil-inst-lowering -allocbox-to-stack -mandatory-inlining -constexpr-limit 3000 -test-constant-evaluable-subset %t/constant_evaluable_profiler_test_silgen.sil > %t/constant_evaluable_profiler_test.sil 2> %t/error-output
//
// RUN: %FileCheck %S/Inputs/constant_evaluable.swift < %t/error-output

// REQUIRES: swift_feature_InferSendableFromCaptures
