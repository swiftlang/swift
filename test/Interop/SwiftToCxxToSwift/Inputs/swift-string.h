#include "StringBridge.h"

// expected-note@+2 {{function 'hello' unavailable (cannot import)}}
// expected-note@+1 {{parameter 'string' unavailable (cannot import)}}
inline void hello(swift::String string) {}
// expected-note@+1 {{'makeString()' has been explicitly marked unavailable}}
inline swift::String makeString() { return swift::String("from C++"); }
inline swift::String echoString(swift::String string) { return string; }
inline swift::String borrowString(const swift::String &string) {
  return string;
}
inline swift::String mutateCopy(swift::String string) {
  string.append(swift::String("!"));
  return string;
}
inline void mutateString(swift::String &string) {
  string.append(swift::String("!"));
}
inline swift::String consumeString(swift::String &&string) {
  string.append(swift::String("!"));
  return string;
}
inline swift::String borrowConstRValue(const swift::String &&string) {
  return string;
}

using StringAlias = swift::String;
using SecondStringAlias = StringAlias;
inline SecondStringAlias echoAlias(SecondStringAlias string) { return string; }
inline swift::String mixedStrings(int count, const swift::String &first,
                                  swift::String second) {
  return count ? first : second;
}
inline swift::String
defaultString(swift::String string = swift::String("default")) {
  return string;
}

namespace Strings {
inline swift::String echo(swift::String string) { return string; }
} // namespace Strings

struct StringFunctions {
  swift::String echo(const swift::String &string) const { return string; }
  static swift::String make() { return makeString(); }
};

struct StringHolder {
  int value = 17;
};
inline swift::String memberEcho(const StringHolder &holder,
                                swift::String string)
    __attribute__((swift_name("StringHolder.echo(self:_:)"))) {
  return holder.value == 17 ? string : swift::String("wrong self");
}

swift::String *stringPointer();
const swift::String &stringReference();
swift::Optional<swift::String> optionalString();
swift::Array<swift::String> stringArray();

struct String {
  int value;
};
inline String echoUnrelatedString(String string) { return string; }

// External source metadata alone does not imply a compatible value layout.
struct __attribute__((external_source_symbol(
    language = "Swift", defined_in = "swift", USR = "s:SS",
    generated_declaration))) TrivialString {
  char _storage[sizeof(void *) * 2];
};
TrivialString echoTrivialString(TrivialString string);

struct __attribute__((external_source_symbol(
    language = "Swift", defined_in = "WrongModule", USR = "s:SS",
    generated_declaration))) WrongModuleString {
  char _storage[sizeof(void *) * 2];
  ~WrongModuleString() {}
};
WrongModuleString echoWrongModuleString(WrongModuleString string);

struct __attribute__((external_source_symbol(
    language = "Swift", defined_in = "swift", USR = "s:SS")))
NonGeneratedString {
  char _storage[sizeof(void *) * 2];
  ~NonGeneratedString() {}
};
NonGeneratedString echoNonGeneratedString(NonGeneratedString string);

struct __attribute__((external_source_symbol(
    language = "Swift", defined_in = "swift", USR = "s:Si",
    generated_declaration))) WrongUSRString {
  char _storage[sizeof(void *) * 2];
  ~WrongUSRString() {}
};
WrongUSRString echoWrongUSRString(WrongUSRString string);

struct __attribute__((
    external_source_symbol(language = "Swift", defined_in = "swift",
                           USR = "s:SS", generated_declaration))) OpaqueString {
  void *_storage;
  ~OpaqueString() {}
};
OpaqueString echoOpaqueString(OpaqueString string);

struct __attribute__((external_source_symbol(
    language = "Swift", defined_in = "swift", USR = "s:SS",
    generated_declaration))) IncompleteString;
void unusedIncompleteString(IncompleteString string);
