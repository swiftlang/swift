swift_add_library(swiftDemangling
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
                    TypeDecoder.cpp
                  C_COMPILE_FLAGS
                    -DLLVM_DISABLE_ABI_BREAKING_CHECKS_ENFORCING=1)

