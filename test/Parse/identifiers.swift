// RUN: %target-typecheck-verify-swift

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
func <#some name#>() {} // expected-error {{editor placeholder in source file}}

// Keywords as identifiers
class switch {} // expected-error {{keyword 'switch' cannot be used as an identifier here}} expected-note {{if this name is unavoidable, use backticks to escape it}} {{7-13=`switch`}}
struct Self {} // expected-error {{keyword 'Self' cannot be used as an identifier here}} expected-note {{if this name is unavoidable, use backticks to escape it}} {{8-12=`Self`}}
protocol enum {} // expected-error {{keyword 'enum' cannot be used as an identifier here}} expected-note {{if this name is unavoidable, use backticks to escape it}} {{10-14=`enum`}}
protocol test {
  associatedtype public // expected-error {{keyword 'public' cannot be used as an identifier here}} expected-note {{if this name is unavoidable, use backticks to escape it}} {{18-24=`public`}}
}
func _(_ x: Int) {} // expected-error {{keyword '_' cannot be used as an identifier here}} // expected-note {{if this name is unavoidable, use backticks to escape it}} {{6-7=`_`}}

// SIL keywords are tokenized as normal identifiers in non-SIL mode.
_ = undef // expected-error {{use of unresolved identifier 'undef'}}
_ = sil // expected-error {{use of unresolved identifier 'sil'}}
_ = sil_stage // expected-error {{use of unresolved identifier 'sil_stage'}}
_ = sil_vtable // expected-error {{use of unresolved identifier 'sil_vtable'}}
_ = sil_global // expected-error {{use of unresolved identifier 'sil_global'}}
_ = sil_witness_table // expected-error {{use of unresolved identifier 'sil_witness_table'}}
_ = sil_default_witness_table // expected-error {{use of unresolved identifier 'sil_default_witness_table'}}
_ = sil_coverage_map // expected-error {{use of unresolved identifier 'sil_coverage_map'}}
_ = sil_scope // expected-error {{use of unresolved identifier 'sil_scope'}}
