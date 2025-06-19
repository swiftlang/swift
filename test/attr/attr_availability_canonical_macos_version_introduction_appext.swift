// RUN: %swift -typecheck -verify -parse-stdlib -module-name Swift -target %target-cpu-apple-macosx10.15 %s -application-extension

@available(OSXApplicationExtension 11, *)
func introducedInAppExtension11_0() { }

@available(OSXApplicationExtension 10.16, *)
func useAppExtension10_16() {
  introducedInAppExtension11_0() // no-warning
}

@available(OSXApplicationExtension 26, *)
func introducedInAppExtension26_0() { }

@available(OSXApplicationExtension 16, *)
func useAppExtension16_0() {
  introducedInAppExtension26_0() // no-warning
}
