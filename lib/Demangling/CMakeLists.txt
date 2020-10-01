add_swift_host_library(swiftDemangling STATIC
  Demangler.cpp
  Context.cpp
  ManglingUtils.cpp
  NodeDumper.cpp
  NodePrinter.cpp
  OldDemangler.cpp
  OldRemangler.cpp
  Punycode.cpp
  Remangler.cpp)
target_compile_definitions(swiftDemangling PRIVATE
  LLVM_DISABLE_ABI_BREAKING_CHECKS_ENFORCING=1)

# NOTE: Runtime libraries that depend on swiftDemangling should define
# SWIFT_INLINE_NAMESPACE to specify the identifier that will be used for an
# inline namespace that will be added around the symbols defined by this
# library. This keeps the demangling symbols in those libraries distinct
# from those in the compiler, which prevents ODR violations in certain
# contexts; for example, on platforms that support statically linking the
# Swift standard library, it allows this to happen safely when the binary
# also links in compiler libraries that may not match exactly with the
# runtime version.
