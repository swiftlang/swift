// RUN: %swift -parse -verify %s

class 你好 {
  class שלום {
    class வணக்கம் {
      class Γειά {
        static func привет() {
          println("hello")
        }
      }
    }
  }
}

你好.שלום.வணக்கம்.Γειά.привет()

// Identifiers cannot start with combining chars.
.́duh() // expected-error{{an identifier cannot begin with this character}} //expected-error{{an identifier cannot begin with this character}} //expected-error{{expected identifier after '.' expression}}

// Combining characters can be used within identifiers.
func s̈pin̈al_tap̈() {}

// Private-use characters aren't valid in Swift source.
() // expected-error{{invalid character in source file}}
