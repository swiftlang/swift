// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil %s -enable-objc-interop -I %S/Inputs/custom-modules/AliasCaseErrorEnum

// REQUIRES: objc_interop

import AliasCaseErrorEnum

// Make sure that we can reference aliases defined in the wrapper type
// which themselves point at aliases inside the nested 'Code' type.

_ = AliasError.realName
_ = AliasError.fakeName