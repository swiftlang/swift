// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -D DEFINITIONS_ONLY -module-name adcs_capture_generic %S/../../validation-test/closure_specialization/adcs_capture_generic.swift -o %t.sil
// RUN: %FileCheck %S/../../validation-test/closure_specialization/adcs_capture_generic.swift < %t.sil
