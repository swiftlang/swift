// RUN: %target-typecheck-verify-swift -verify %S/Inputs/class_missing_init_multi1.swift %S/Inputs/class_missing_init_multi2.swift
// RUN: %target-typecheck-verify-swift -verify %S/Inputs/class_missing_init_multi2.swift %S/Inputs/class_missing_init_multi1.swift

// RUN: %target-typecheck-verify-swift -verify -primary-file %S/Inputs/class_missing_init_multi1.swift %S/Inputs/class_missing_init_multi2.swift
// RUN: %target-typecheck-verify-swift -verify %S/Inputs/class_missing_init_multi1.swift -primary-file %S/Inputs/class_missing_init_multi2.swift

// RUN: %target-typecheck-verify-swift -verify -primary-file %S/Inputs/class_missing_init_multi2.swift %S/Inputs/class_missing_init_multi1.swift
// RUN: %target-typecheck-verify-swift -verify %S/Inputs/class_missing_init_multi2.swift -primary-file %S/Inputs/class_missing_init_multi1.swift
