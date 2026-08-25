// RUN: %target-typecheck-verify-swift -I %S%{fs-sep}Inputs -I %swift_src_root/lib/ClangImporter/SwiftBridging -cxx-interoperability-mode=default -disable-availability-checking -Xcc -Wno-nullability-completeness -verify-ignore-unknown -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}libkern-ownership.hpp -enable-experimental-feature LibkernOwnershipConventions

// REQUIRES: swift_feature_LibkernOwnershipConventions

import LibkernOwnership

let service = Service.withID(3)
_ = service.getProvider() // no warning expected

_ = service.copyService() // no warning expected

_ = OSIterator.getIterator() // no warning expected

let _ = NastyService.toRetainOrNotToRetain()
