# Strict language feature enablement

By default, if an unrecognized feature name is specified with the
`-enable-upcoming-feature` or `-enable-experimental-feature` flags, the compiler
will ignore it without emitting a diagnostic since some projects must be
simultaneously compatible with multiple versions of the language and toolchain.
However, this warning group can be enabled to opt-in to detailed diagnostics
about misspecified features.
