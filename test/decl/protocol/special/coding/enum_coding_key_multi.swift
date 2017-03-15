// RUN: %target-swift-frontend -typecheck -verify %S/Inputs/enum_coding_key_multi1.swift %S/Inputs/enum_coding_key_multi2.swift
// RUN: %target-swift-frontend -typecheck -verify %S/Inputs/enum_coding_key_multi2.swift %S/Inputs/enum_coding_key_multi1.swift
