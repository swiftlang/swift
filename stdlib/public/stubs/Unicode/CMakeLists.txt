
# Embedded Swift Unicode library
if(SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB)
  add_custom_target(embedded-unicode)
  add_dependencies(embedded-libraries embedded-unicode)

  add_embedded_swift_target_library(
    embedded-unicode
    swiftUnicodeDataTables
    INSTALL_BINARY

    UnicodeData.cpp
    UnicodeEmoji.cpp
    UnicodeGrapheme.cpp
    UnicodeNormalization.cpp
    UnicodeScalarProps.cpp
    UnicodeWord.cpp

    C_COMPILE_FLAGS -nostdinc++
    SKIP_MOD_REGEX "-windows-msvc$"
    SKIP_ARCH_REGEX "avr"
    ARCHITECTURE_KEY mod
    INSTALL_IN_COMPONENT stdlib
  )
endif()
