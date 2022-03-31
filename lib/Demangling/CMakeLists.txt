set(swift_demangling_compile_flags
  LLVM_DISABLE_ABI_BREAKING_CHECKS_ENFORCING=1
  SWIFT_SUPPORT_OLD_MANGLING=1
  SWIFT_STDLIB_HAS_TYPE_PRINTING=1)

if(SWIFT_RUNTIME_CRASH_REPORTER_CLIENT)
  list(APPEND swift_demangling_compile_flags
      "-DSWIFT_HAVE_CRASHREPORTERCLIENT=1")
endif()

add_swift_host_library(swiftDemangling STATIC
  Demangler.cpp
  Context.cpp
  ManglingUtils.cpp
  NodeDumper.cpp
  NodePrinter.cpp
  OldDemangler.cpp
  OldRemangler.cpp
  Punycode.cpp
  Remangler.cpp
  Errors.cpp
  CrashReporter.cpp)
target_compile_definitions(swiftDemangling PRIVATE
  ${swift_demangling_compile_flags})

# NOTE: Runtime libraries that depend on swiftDemangling should define
# SWIFT_INLINE_NAMESPACE to specify the identifier that will be used for an
# inline namespace that will be added around the symbols defined by this
# library. This keeps the demangling symbols in those libraries distinct
# from those in the compiler, which prevents ODR violations in certain
# contexts; for example, on platforms that support statically linking the
# Swift standard library, it allows this to happen safely when the binary
# also links in compiler libraries that may not match exactly with the
# runtime version.
