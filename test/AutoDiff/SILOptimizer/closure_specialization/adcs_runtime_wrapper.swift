// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -D DEFINITIONS_ONLY -module-name adcs_runtime_wrapper %S/../../validation-test/closure_specialization/adcs_runtime_wrapper.swift -o %t.sil
// RUN: %FileCheck %S/../../validation-test/closure_specialization/adcs_runtime_wrapper.swift < %t.sil
