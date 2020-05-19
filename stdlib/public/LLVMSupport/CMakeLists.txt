add_swift_target_library(swiftLLVMSupport OBJECT_LIBRARY
    ErrorHandling.cpp
    Hashing.cpp
    MemAlloc.cpp
    SmallPtrSet.cpp
    SmallVector.cpp
    StringRef.cpp
  INSTALL_IN_COMPONENT
    never_install)
