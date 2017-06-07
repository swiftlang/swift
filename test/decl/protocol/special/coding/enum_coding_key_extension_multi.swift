// RUN: %target-swift-frontend -typecheck -verify %S/Inputs/enum_coding_key_extension_multi1.swift %S/Inputs/enum_coding_key_extension_multi2.swift %S/Inputs/enum_coding_key_extension_multi3.swift
// RUN: %target-swift-frontend -typecheck -verify %S/Inputs/enum_coding_key_extension_multi1.swift %S/Inputs/enum_coding_key_extension_multi3.swift %S/Inputs/enum_coding_key_extension_multi2.swift
// RUN: %target-swift-frontend -typecheck -verify %S/Inputs/enum_coding_key_extension_multi2.swift %S/Inputs/enum_coding_key_extension_multi1.swift %S/Inputs/enum_coding_key_extension_multi3.swift
// RUN: %target-swift-frontend -typecheck -verify %S/Inputs/enum_coding_key_extension_multi2.swift %S/Inputs/enum_coding_key_extension_multi3.swift %S/Inputs/enum_coding_key_extension_multi1.swift
// RUN: %target-swift-frontend -typecheck -verify %S/Inputs/enum_coding_key_extension_multi3.swift %S/Inputs/enum_coding_key_extension_multi1.swift %S/Inputs/enum_coding_key_extension_multi2.swift
// RUN: %target-swift-frontend -typecheck -verify %S/Inputs/enum_coding_key_extension_multi3.swift %S/Inputs/enum_coding_key_extension_multi2.swift %S/Inputs/enum_coding_key_extension_multi1.swift
