// RUN: %target-swift-frontend -emit-sil -verify -O -assert-config Debug       %s -o /dev/null
// RUN: %target-swift-frontend -emit-sil -verify -O -assert-config Release     %s -o /dev/null
// RUN: %target-swift-frontend -emit-sil -verify -Onone -assert-config Debug   %s -o /dev/null
// RUN: %target-swift-frontend -emit-sil -verify -Onone -assert-config Release %s -o /dev/null

func assertionFailure_isNotNoreturn() -> Int {
  _ = 0 // Don't implicitly return the assertionFailure call.
  assertionFailure("")
} // expected-error {{missing return in global function expected to return 'Int'}}

func rdar183643880Reproducer(item: String?) {
  guard let item else {
    assertionFailure("")
  } // expected-error {{'guard' body must not fall through}}

  print(".some: \(item)")
}
