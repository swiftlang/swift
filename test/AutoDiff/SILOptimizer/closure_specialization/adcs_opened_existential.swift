// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -D DEFINITIONS_ONLY -module-name adcs_opened_existential %S/../../validation-test/closure_specialization/adcs_opened_existential.swift -o %t.sil
// RUN: %FileCheck %S/../../validation-test/closure_specialization/adcs_opened_existential.swift < %t.sil
