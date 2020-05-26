// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -verify -enable-objc-interop -I %S/Inputs/custom-modules/RedeclaredErrorEnum
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -verify -enable-objc-interop -I %S/Inputs/custom-modules/RedeclaredErrorEnum -DIMPORT_BASE
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -verify -enable-objc-interop -I %S/Inputs/custom-modules/RedeclaredErrorEnum -DIMPORT_BASE -Xcc -DNO_IMPORT_BASE_FROM_REDECLARED

#if IMPORT_BASE
import Base
#endif

import Redeclared

// Referencing this error type (defined in Base, redeclared in Redeclared)
// used to cause a compiler crash (rdar://problem/45414271).
_ = SomeError.self
_ = SomeError.Code.self

_ = Redeclared.SomeError.self
_ = Base.SomeError.self
