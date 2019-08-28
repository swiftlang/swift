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
