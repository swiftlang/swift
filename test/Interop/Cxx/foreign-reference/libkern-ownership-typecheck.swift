// RUN: %target-typecheck-verify-swift -I %S%{fs-sep}Inputs -I %swift_src_root/lib/ClangImporter/SwiftBridging -cxx-interoperability-mode=default -disable-availability-checking -Xcc -Wno-nullability-completeness -verify-ignore-unknown -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}libkern-ownership.hpp

import LibkernOwnership

let service = Service.withID(3)
_ = service.getProvider() 
// expected-warning@-1 {{cannot infer ownership of foreign reference value returned by 'getProvider()'}}

_ = service.copyService() // no warning expected

let _ = NastyService.toRetainOrNotToRetain()
