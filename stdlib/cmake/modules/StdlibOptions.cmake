include_guard(GLOBAL)

option(SWIFT_STDLIB_SUPPORT_BACK_DEPLOYMENT
       "Support back-deployment of built binaries to older OS versions."
       TRUE)

option(SWIFT_STDLIB_SHORT_MANGLING_LOOKUPS
       "Build stdlib with fast-path context descriptor lookups based on well-known short manglings."
       TRUE)

option(SWIFT_STDLIB_HAS_TYPE_PRINTING
       "Build stdlib with support for printing user-friendly type name as strings at runtime"
       TRUE)

option(SWIFT_STDLIB_BUILD_PRIVATE
       "Build private part of the Standard Library."
       TRUE)
