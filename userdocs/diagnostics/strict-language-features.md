# Strict language feature enablement (StrictLanguageFeatures)

Warnings for unrecognized feature names in `-enable-upcoming-feature` or
`enable-experimental-feature`.


## Overview

By default, if an unrecognized feature name is specified with the `-enable-upcoming-feature` or
`-enable-experimental-feature` flags, the compiler will ignore it without emitting a diagnostic
since some projects must be simultaneously compatible with multiple versions of the language and
toolchain. This can, however, lead to misspecified features. To diagnose these cases instead, enable
`StrictLanguageFeatures`.
