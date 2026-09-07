// RUN: %target-typecheck-verify-swift -I %S%{fs-sep}Inputs -I %swift_src_root/lib/ClangImporter/SwiftBridging -cxx-interoperability-mode=default -Xcc -Wno-nullability-completeness -verify-ignore-unknown -verify-ignore-unrelated -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}libkern-ownership.hpp -enable-experimental-feature LibkernOwnershipConventions

// REQUIRES: swift_feature_LibkernOwnershipConventions

import LibkernOwnership

if #available(SwiftStdlib 5.8, *) {
  let service = Service.withID(3)
  _ = service.getProvider() // no warning expected

  _ = service.copyService() // no warning expected

  _ = OSIterator.getIterator() // no warning expected

  _ = NastyService.toRetainOrNotToRetain()

  service.consumeMyself() // expected-error {{'consumeMyself()' is unavailable: LIBKERN_CONSUMES_THIS annotation is not supported}}

  Service.consumesService(service) // expected-error {{'consumesService' is unavailable: LIBKERN_CONSUMED annotation is not supported}}

  let derivedService = DerivedService.derivedWithID(9)

  derivedService.consumeMyself() // expected-error {{'consumeMyself()' is unavailable: LIBKERN_CONSUMES_THIS annotation is not supported}}
}
