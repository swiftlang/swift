
add_swift_library(swiftSwiftPrivateThreadExtras
                  ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES}
                  IS_STDLIB

                  # NOTE: this file should be listed first, the module name is
                  # inferred from the filename.
                  SwiftPrivateThreadExtras.swift
                  SwiftPrivateThreadBarrier.swift

                  SWIFT_MODULE_DEPENDS_IOS Darwin
                  SWIFT_MODULE_DEPENDS_OSX Darwin
                  SWIFT_MODULE_DEPENDS_TVOS Darwin
                  SWIFT_MODULE_DEPENDS_WATCHOS Darwin

                  SWIFT_MODULE_DEPENDS_LINUX Glibc
                  SWIFT_MODULE_DEPENDS_FREEBSD Glibc
                  SWIFT_MODULE_DEPENDS_CYGWIN Glibc
                  SWIFT_MODULE_DEPENDS_HAIKU Glibc

                  SWIFT_MODULE_DEPENDS_WINDOWS MSVCRT

                  SWIFT_COMPILE_FLAGS
                    -I${CMAKE_CURRENT_SOURCE_DIR}

                  INSTALL_IN_COMPONENT
                    stdlib-experimental)

