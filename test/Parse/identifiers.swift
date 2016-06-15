// RUN: %target-parse-verify-swift

func my_print<T>(_ t: T) {}

class 你好 {
  class שלום {
    class வணக்கம் {
      class Γειά {
        class func привет() {
          my_print("hello")
        }
      }
    }
  }
}

你好.שלום.வணக்கம்.Γειά.привет()

// Identifiers cannot start with combining chars.
_ = .́duh() // expected-error {{use of unresolved operator '.́'}} // expected-error{{use of unresolved identifier 'duh'}}

// Combining characters can be used within identifiers.
func s̈pin̈al_tap̈() {}

// Private-use characters aren't valid in Swift source.
() // expected-error{{invalid character in source file}} {{1-4= }}

// Placeholders are recognized as identifiers but with error.
func <#some name#>() {} // expected-error 2 {{editor placeholder in source file}}
