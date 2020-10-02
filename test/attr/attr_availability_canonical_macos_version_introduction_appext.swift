// RUN: %swift -typecheck -verify -parse-stdlib -module-name Swift -target x86_64-apple-macosx10.15 %s -application-extension

@available(OSXApplicationExtension 11, *)
func introducedInAppExtension11_0() { }

@available(OSXApplicationExtension 10.16, *)
func useAppExtension() {
  introducedInAppExtension11_0() // no-warning
}
