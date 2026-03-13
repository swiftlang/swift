// RUN: %target-typecheck-verify-swift -enable-library-evolution

#if hasFeature(LibraryEvolution)
#else
#error("Missing feature")
#endif
