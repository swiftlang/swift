// RUN: %target-typecheck-verify-swift

// Don't emit a warning when macatalyst is used in
// availability markup.

@available(iOS, introduced: 52.0)
@available(macCatalyst, introduced: 53.0)
func long_form_availability() {}

@available(macCatalyst, unavailable) // no-warning
@available(macCatalystApplicationExtension, unavailable) // no-warning
func unavailable_on_catalyst() {}

@available(macCatalyst 53.0, macCatalystApplicationExtension 54.0, *) // no warning
func short_form_availability() {}




