Status of Unicode Work
======================

- Making a universal String type

  - to be able to store all models of `Unicode` in a `String` we need a way to
    type-erase them.

  - `Unicode`
    as
    [currently defined](https://github.com/apple/swift/blob/unicode-rethink/test/Prototypes/Unicode.swift#L16) in
    is full of associated types, and thus cannot be an existential.
  
  - Two options: existentials and manual type erasure with classes
  
  - Manual type erasure implies an extra allocation whereas existentials may fit
    in an inline buffer, so opt for existentials
  
  - Conclusions:
  
    - Create an existential-able
      protocol,
      [AnyUnicode](https://github.com/apple/swift/blob/unicode-rethink/test/Prototypes/AnyUnicode.swift) to
      which all things stored in a `String` will conform.

    - Code units are expressed by `AnyUnicode` as `UInt32`s, possibly
      zero-extended.
      
      
    - Eventually Unicode (or something like it) may refine `AnyUnicode`
    
      - `Unicode` would still expose an associated `CodeUnits` type and
        `codeUnits` property.
        
      - A default implementation of `AnyUnicode`'s `codeUnits` property would be
        trivially derived from `Unicode`'s `codeUnits` (if the name clash is an
        issue, rename something; I'm not attached).

    - If `String` stores only UTF-16-compatible encodings, its associated
      `Encoding` type should be `UTF16`.
    
    - If `String` stores *any* `Unicode` (as opposed to only UTF-16-compatible
      encodings) and conforms to `Unicode`, then one of the following must hold:
      
      - `Unicode` drops its associated `Encoding` type and instead
        exposes a property that is an instance of `AnyUnicodeEncoding.Type`
        
      - `Unicode` has an associated `Encoding` type and  exposes a property that is
        an instance of `Encoding`, and 
        - all `static` methods of `UnicodeEncoding` become instance methods
        - String's associated encoding type becomes `UniversalUnicodeEncoding`,
          a “universal” model of `UnicodeEncoding` that stores and dispatches to
          an instance of `AnyUnicodeEncoding`, with a `CodeUnit` type of `UInt32`.
      
      I *think* I prefer the latter; the all-static API of `UnicodeEncoding` has
      always bugged me, but you will have to see.
      
    - Lack of generalized existentials makes it hard to express things like "Any
      random-access collection of unsigned integers", so following an awkward
      pattern:
      
      - Models of `protocol AnyCodeUnits_` represent code units to
        `AnyUnicode`.  This can't conform to `RandomAccessCollection` because it
        would inherit associated types, even though it nails them all down.
        
      - `struct AnyCodeUnits` wraps an `AnyCodeUnits_` and adds the missing
        `RandomAccessCollection` constraint.
        
      - `struct AnyCodeUnitsZeroExtender` wraps any random access collection of
        unsigned integers and presents it as both `RandomAccessCollection` and
        `AnyCodeUnits_`.  This adapter would be used by the default
        implementation of `AnyUnicode`'s `codeUnits` property.
    
TODO
----

- Create a UTF-16 string storage type
    - Derive from _SwiftNativeNSString
    - Similar to _ContiguousArrayBuffer<Element>
    - Note the use of [tail-allocated element builtins](https://github.com/apple/swift/blob/master/stdlib/public/core/ContiguousArrayBuffer.swift#L173)
    - [This class](https://gist.github.com/dabrahams/9db67f06a693ed8de7cb2a36ead29bf6#file-stringtest-swift-L53) 
      might be useful to get there, but note that you can't use ManagedBuffer.
    
- Create a Latin-1 string storage type

- Create empty string storage similar
  to
  [_EmptyArrayStorage](https://github.com/apple/swift/blob/master/stdlib/public/core/ContiguousArrayBuffer.swift#L21)

- Ressurrect what was
  reverted
  [here](https://github.com/apple/swift/commit/9a6f3ef8fea3d8eb16dfbd8e24ce183364c67403) and
  make it work.

  
Code That Might be Useful
-------------------------

Miscellaneous stuff is
in
[this gist](https://gist.github.com/dabrahams/9db67f06a693ed8de7cb2a36ead29bf6)
