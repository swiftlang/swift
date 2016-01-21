# Symbolic Type References

*Symbolic type references* are compact represenations containing the
minimum amount of information about type relationships primarily for the
purposes of reflection.

## Encoding

Thees are encoded with a mostly textual mangling. Single characters
denote the start of a mangling node but 32-bit integers can also be
directly embedded.

```
typeref-symbol ::= prefix typeref

typeref ::= 'b' relative-offset-to-name                          // Builtin type
typeref ::= 'n' relative-offset-to-metadata                      // Internal nominal type
typeref ::= 'N' relative-offset-to-name                          // External nominal type
typeref ::= 'g' relative-offset-to-metadata count typeref-list   // Internal nound generic type
typeref ::= 'G' relative-offset-to-name count typeref-list       // External bound generic type
typeref ::= '?' index depth                                      // Generic parameter
typeref ::= 't' num-elements typeref-list                        // Tuple type
typeref ::= 'f' func-representation count typeref typeref        // Function type
typeref ::= 'p' has-class-constrained-flag? count protocol-list  // Protocol composition
typeref ::= 'm' typeref                                          // Metatype
typeref ::= 'e' count protocol-list                              // Existential metatype

has-class-constrainged-flag ::= 'c'

func-representation ::= 't' // Thin
func-representation ::= 'T' // Thick
func-representation ::= 'b' // Block

base-255-i32    ::= <32-bit integer, variable length base-255 encoded>

count           ::= base-255-i32
index           ::= base-255-i32
depth           ::= base-255-i32
relative-offset ::= base-255-i32
```

### Relative Offset, Index, and Depth tokens

`Relative offset` tokens are the 32-bit integers packed into the type
reference -- not a string of digits. These are used to efficiently
reference other types internally in the current binary image, avoiding
unnecessarily embedding redundant copies of type manglings, and
"pointing" to a mangled string for external type references. This uses a
base-255 encoding to avoid nulls in the sequence of bytes.

The offset value is relative to the address of the offset itself so that
a base address doesn't need to be threaded when parsing bytes.

For external references to types defined in other images, the relative
offset takes you to the mangled name string for symbol lookup.

`Count`, `Index`, and `Depth` tokens are encoded this way as well.

### Flags

The flags of the field record is a 32-bit unsigned integer marking some
statically known facts about the field's type, such as reference ownership.

#### Flag bits

TODO
