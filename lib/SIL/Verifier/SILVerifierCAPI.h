//===--- SILVerifierCAPI.h ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// C API for calling the SILVerifier from a dynamically loaded dylib.
///
//===----------------------------------------------------------------------===//

#ifdef __cplusplus
extern "C" {
#endif

#ifdef SILVERIFIERCAPI_WEAK
#error "SILVERIFIERCAPI_WEAK defined?!"
#endif

#ifdef __APPLE__
#define SILVERIFIERCAPI_WEAK __attribute__((weak_import))
#elif LINUX
#define SILVERIFIERCAPI_WEAK __attribute__((weak))
#else
#error "Unhandle platform?!"
#endif

#define SILVERIFIERCAPI_NDEBUG

/// Pass in a type erased SILFunction. Inside this API, we convert back to
/// SILFunction and run the verifier.
SILVERIFIERCAPI_WEAK extern "C" void
SILVerifierCAPI_SILFunction_verify(void *silFunction, bool singleFunction);

/// Pass in a type erased SILModule. Inside this API, we convert back to
/// SILModule and run the verifier.
SILVERIFIERCAPI_WEAK extern "C" void
SILVerifierCAPI_SILModule_verify(void *silModule);

/// Pass in a type erased SILFunction. Inside this API, we convert back to
/// SILFunction and verifying critical edges.
SILVERIFIERCAPI_WEAK extern "C" void
SILVerifierCAPI_SILFunction_verifyCriticalEdges(void *silFunction);

SILVERIFIERCAPI_WEAK extern "C" void
SILVerifierCAPI_SILProperty_verify(void *silProperty, void *silModule);

SILVERIFIERCAPI_WEAK extern "C" void
SILVerifierCAPI_SILVTable_verify(void *silVTable, void *silModule);

SILVERIFIERCAPI_WEAK extern "C" void
SILVerifierCAPI_SILWitnessTable_verify(void *silWitnessTable, void *silModule);

SILVERIFIERCAPI_WEAK extern "C" void
SILVerifierCAPI_SILDefaultWitnessTable_verify(void *silWitnessTable,
                                              void *silModule);

SILVERIFIERCAPI_WEAK extern "C" void
SILVerifierCAPI_SILGlobalVariable_verify(void *silGlobalVariable);

#undef SILVERIFIERCAPI_WEAK
#ifdef __cplusplus
}
#endif
