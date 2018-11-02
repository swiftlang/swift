add_swift_host_library(swiftDemangling
                  STATIC
                    Demangler.cpp
                    Context.cpp
                    ManglingUtils.cpp
                    NodeDumper.cpp
                    NodePrinter.cpp
                    OldDemangler.cpp
                    OldRemangler.cpp
                    Punycode.cpp
                    Remangler.cpp
                    TypeDecoder.cpp)
target_compile_definitions(swiftDemangling
                           PRIVATE
                             LLVM_DISABLE_ABI_BREAKING_CHECKS_ENFORCING=1)

