### Background ###

Swift's diagnostic style is inherited from [Clang][], which itself was loosely based on [GCC][]'s. These diagnostics aim to fit a terse but clear description of an issue into a single line, ideally suitable for display both on the command line and in IDEs. Additional information is provided via "notes", which are always attached to a primary diagnostic.

The Clang site has [an older comparison][comparison] between Clang's diagnostics and GCC's that shows where the attention to detail has been in that compiler.

This document describes several *current* guidelines for diagnostics in the Swift compiler and accompanying tools. It does not discuss potential future directions for diagnostics, such as following the examples of the [Elm][] or [Rust][] compilers to provide more information.

  [Clang]: http://clang.llvm.org
  [GCC]: https://gcc.gnu.org
  [comparison]: http://clang.llvm.org/diagnostics.html
  [Elm]: http://elm-lang.org/blog/compiler-errors-for-humans
  [Rust]: https://blog.rust-lang.org/2016/08/10/Shape-of-errors-to-come.html


### Errors vs. Warnings ###

Swift diagnostics are classified into errors, warnings, and notes. Notes are always considered "attached" to the immediately preceding error or warning, so the primary distinction is between errors and warnings. In the Swift compiler, a particular diagnostic should be a warning if the intent of the code is clear *and* it won't immediately result in a crash. This allows users to continue to build during refactoring without having to clean up all issues first.

For a warning, consider whether there might be a legitimate need to write the code in question, even if it's rare. In this case, there should be a way to silence the warning, and a note with a fix-it suggesting this. For example, the "unused function result" warning suggests assigning to `_` to make it clear that this is intentional.

Unlike Clang and many other compilers, Swift compiler warnings cannot be disabled as part of the invocation or through a standard construct in the source. This was a deliberate choice in order to keep the language from fracturing into dialects, but has been controversial throughout Swift's history.

Clang also has a kind of diagnostic called a "remark", which represents information about the build that does not indicate an issue. Swift does not currently have remarks, but there's no reason why it couldn't.


### Grammar and Phrasing ###

- Swift diagnostics should be a single phrase or sentence, with no period at the end. If it's important to include a second idea in the diagnostic, use a semicolon to separate the two parts.

- Swift diagnostics are written in a terse abbreviated style similar to English newspaper headlines or recipes. Omit words that make the diagnostic longer without adding information, such as grammatical words like "the".

- *Do* include information that shows that the compiler understands the code. For example, referring to "instance method 'foo(bar:)'" is usually unnecessary, but may increase implicit trust in the compiler and decrease the developer's mental load.

  - *Don't* include information that might show that the compiler *doesn't* understand the code. For example, don't *assume* that this particular value is an instance method when it might be a property with a function type. However, *do* include details that might show how the compiler is *misunderstanding* the code.

- If there is a plausible fix or likely intended meaning, include that in the diagnostic whether or not there's a fix-it.

- When applicable, phrase diagnostics as rules rather than reporting that the compiler failed to do something. Example:

  - Normal: "cannot call 'super.init' outside of an initializer"
  - Better: "'super.init' cannot be called outside of an initializer"

- When referring to attributes by name, use *either* "the 'foo' attribute" or "'@foo'", rather than "the '@foo' attribute".

- Match the tone and phrasing of other diagnostics. Some common phrases:

  - "\<noun> '\<name>'" (e.g. "type 'Set<Int>'")
  - "value of type \<type>"
  - "did you mean...?"
  - "...to silence this warning"
  - "...here" (for a purely locational note)


### Locations and Highlights ###

- Diagnostics are always emitted at a particular line and column. Try to be specific.

- Use highlights to build on the single location of the diagnostic, but use them sparingly. A diagnostic with *everything* on the line highlighted is no better than a diagnostic with nothing highlighted.


### Fix-its ###

- Fix-its can span multiple lines, but may not be displayed in all contexts if they do. (In particular, the command-line display of a diagnostic currently only shows the line the diagnostic is on.)

- Fix-its must be in the same file as the diagnostic they are attached to. (However, a note can be in a different file than the primary error or warning.)

- If a fix-it is placed on an error or warning, it must be the single, obvious, and very likely correct way to fix the issue.

  - Ideally, the compiler or other tool will recover as if the user had applied the fix-it.

- Conversely, if a note has a fix-it, the note should describe the action the fix-it is taking and why.

- If a diagnostic has multiple notes with fix-its, the different notes should be treated as alternatives. In general, the first option should be the safest one. (It's also okay to have additional notes that do not provide fix-its, but don't drown the developer in notes.)

- If a warning or error has a fix-it, its notes should not have fix-its.

- Try to find something better than "add parentheses" to indicate that a warning should be silenced. Parentheses don't have anything to do with the actual problem, and if too many warnings do this the developer may end up silencing more than they meant to.

- Use Xcode-syntax placeholders in fix-its as necessary: `<#placeholder#>`. It's better to offer a fix-it with a placeholder in it than no fix-it at all.

The correct spelling of this feature is "fix-it" rather than "fixit". In [camelcased][] contexts, use "FixIt" or "fixIt".

  [camelcased]: https://en.wikipedia.org/wiki/Camel_case


### "Editor Mode" ###

The Swift compiler has a setting (under LangOptions) called `DiagnosticsEditorMode`. When set, diagnostics should be customized for an interactive editor that can display and apply complex fix-its, and worry less about the appearance in build logs and command-line environments.

Most diagnostics have no reason to change behavior under editor mode. An example of an exception is the "protocol requirements not satisfied diagnostic"; on the command line, it may be better to show all unsatisfied requirements, while in an IDE a single multi-line fix-it would be preferred.


### Format Specifiers ###

(This section is specific to the Swift compiler's diagnostic engine.)

- `%0`, `%1`, etc - Formats the specified diagnostic argument based on its type.

- `%select{a|b|c}0` - Chooses from a list of alternatives, separated by vertical bars, based on the value of the given argument. In this example, a value of 2 in diagnostic argument 0 would result in "c" being output. The argument to the %select may be an integer, enum, or StringRef. If it's a StringRef, the specifier acts as an emptiness check.

- `%s0` - Produces an "s" if the given argument is anything other than 1, as meant for an English plural. This isn't particularly localizable without a more general `%plural` form, but most diagnostics try to avoid cases where a plural/singular distinction would be necessary in the first place.

- `%error` - Represents a branch in a `%select` that should never be taken. In debug builds of the compiler this produces an assertion failure.

- `%%` - Emits a literal percent sign.


### Diagnostic Verifier ###

(This section is specific to the Swift compiler's diagnostic engine.)

If the `-verify` frontend flag is used, the Swift compiler will check emitted diagnostics against specially formatted comments in the source. This feature is used extensively throughout the test suite to ensure diagnostics are emitted with the correct message and source location.

An expected diagnostic is denoted by a comment which begins with `expected-error`, `expected-warning`, `expected-note`, or `expected-remark`. It is followed by:

- (Optional) Location information. By default, the comment will match any diagnostic emitted on the same line. However, it's possible to override this behavior and/or specify column information as well. `// expected-error@-1 ...` looks for an error on the previous line, `// expected-warning@+1:3 ...` looks for a warning on the next line at the third column, and `// expected-note@:7 ...` looks for a note on the same line at the seventh column.

- (Optional) A match count which specifies how many times the diagnostic is expected to appear. This may be a positive integer or `*`, which allows for zero or more matches. The match count must be surrounded by whitespace if present. For example, `// expected-error 2 ...` looks for two matching errors, and `// expected-warning * ...` looks for any number of matching warnings.

- (Required) The expected error message. The message should be enclosed in double curly braces and should not include the `error:`/`warning:`/`note:`/`remark:` prefix. For example, `// expected-error {{invalid redeclaration of 'y'}}` would match an error with that message on the same line. The expected message does not need to match the emitted message verbatim. As long as the expected message is a substring of the original message, they will match.

- (Optional) Expected fix-its. These are each enclosed in double curly braces and appear after the expected message. An expected fix-it consists of a column range followed by the text it's expected to be replaced with. For example, `let r : Int i = j // expected-error{{consecutive statements}} {{12-12=;}}` will match a fix-it attached to the consecutive statements error which inserts a semicolon at column 12, just after the 't' in 'Int'. The special {{none}} specifier is also supported, which will cause the diagnostic match to fail if unexpected fix-its are produced.


### Public Diagnostics ###

In order to make it easy to search the web or documentation for information about a diagnostic, some diagnostics are designated "public" by associating them with a "public diagnostic". A public diagnostic may correspond to multiple closely related compiler-internal diagnostics. It provides a unique identifier which can unambiguously refer to the diagnostic and remain stable across compiler versions. 

Diagnostics which should be made public include errors and warnings which a Swift programmer might reasonably encounter while using officially supported language features. Diagnostics which should not be made public include those which enforce restrictions on private/underscored features (e.g. `@_implements`), and those which only compiler developers are likely to encounter (e.g. SIL parsing errors). In general, notes should not be made public as they are considered part of their parent error or warning. However, _rare_ exceptions may be made for notes which are independent of a specific error and warning (e.g. constraint fixes diagnosed as notes in ambiguous cases when typechecking).

Public diagnostics are defined in `PublicDiagnostics.def`. Currently, a public diagnostic is defined by:

- A unique name, which serves as its identifier

- The compiler version it was introduced in

-  (optionally) The compiler version it was removed in (i.e. the compiler version which no longer emits any internal diagnostics associated with it)

In the future, this may expand to also include documentation and reference material which explains the diagnostic in greater detail, similar to `rustc`'s `explain` command. 

It's important that once a diagnostic is made public it remains stable so that documentation and other resources which reference it remain up-to-date and accurate across different versions of the compiler. To maintain stability, a public diagnostic:

- _Cannot_ be removed entirely once shipped as part of a compiler release. However, if the compiler no longer emits any corresponding compiler-internal diagnostics, it may be marked as removed in a specific version.

- _Can_ add, remove, or modify associated compiler-internal diagnostics. However, any changes to compiler-internal diagnostics should not substantially change the meaning of the public diagnostic. A good rule of thumb here is to look at existing posts/blogs/documentation related to the diagnostic, and only make the changes if they don't invalidate those resources. If there's any doubt, err on the side of marking the old public diagnostic as removed and introducing a new one.

Because introducing a new public diagnostic requires this commitment to stability, new compiler-internal diagnostics which meet the criteria to be designated public are not required to do so immediately. Sometimes, it may be better to iterate on the diagnostics of a new feature for a period of time before stabilizing them. However, the intent is that every diagnostic which meets the criteria will eventually be declared public.

### Public Diagnostic Naming ###

Public diagnostic names are displayed at the end of a diagnostic message in command line output, enclosed in square brackets.

```
error: invalid redeclaration of 'y' [InvalidRedeclaration]
```

- Names should be written in UpperCamelCase.

- Names should not be longer than 2-3 words wherever possible. Longer names make it harder to fit diagnostic messages on a single line.

- Names should not use terminology which is internal to the compiler (e.g. nominal types, name mangling). Generally, a term of art should only be used if it is defined in _The Swift Programming Language_.

- Names should not include "Error", "Warning", "Note", or other words which only convey the diagnostic type without additional information.

- Names may include abbreviations, but only where the meaning is obvious and they are necessary to maintain a reasonable length. Reasonable abbreviations include shortening "Argument" to "Arg" or "Reference" to "Ref". Avoid simply removing vowels from common words. For example, don't shorten "Invalid" to "Invld".

- Names should omit needless words like "a", "the", "in", etc.

- Names should, when possible, include important key words from the diagnostic message.
