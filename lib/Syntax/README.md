# Swift Syntax and Structured Editing Library

Welcome to lib/Syntax!

This library implements data structures and algorithms for dealing
with Swift syntax. The immediate goal of the library is to provide
infrastructure for the Swift Migrator and a first-class formatting
tool, `swift-format`.

You can read more about the status of the library's implementation at the
[Syntax Status Page](Status.md).

## APIs

### Make APIs

Make APIs are for creating new syntax nodes in a single call. Although you need
to provide all of the pieces of syntax to these APIs, you are free to use
"missing" placeholders as substructure. Make APIs return freestanding syntax
nodes and do not establish any parental relationships.

#### The SyntaxFactory

The `SyntaxFactory` embodies the Make APIs and is the one-stop shop for creating
new syntax nodes and tokens in a single call. There are two main Make APIs
exposed for each Syntax node: making the node with all of the pieces or none of
them. For example, a `StructDeclSyntax` node has a `makeStructDeclSyntax` and
`makeBlankStructDeclSyntax` for those two cases respectively.

**Example**

```c++
```

### With APIs

With APIs are essentially "setters" on Syntax nodes but, because they are
immutable, return new Syntax nodes with only the specified substructure
replaced. Raw backing storage is shared as much as possible.

### Builder APIs

Builder APIs are provided for building up syntax incrementally as it appears. At
any point in the building process, you can call `build()` and get a reasonably
formed Syntax node (i.e. with no raw `nullptr`s) using what you've provided to
the builder so far. Anything that you haven't supplied is marked as "missing".

**Example**

```c++
StructDeclSyntaxBuilder Builder;

Builder.useStructKeyword(StructKeyword);

Builder.useLeftBraceToken(ParsedLeftBrace)
  .useRightBraceToken(ParsedRightBrace);

auto StructWithoutIdentifier = Builder.build();
StructWithoutIdentifier.print(llvm::outs());

// struct {}

// Whoops! Forgot an identifier.

auto MyStructID = SyntaxFactory::createIdentifier("MyStruct", {}, Trivia::spaces(1));
Builder.useIdentifier(MyStructID);

auto StructWithIdentifier = Builder.build();
StructWithIdentifier.print(llvm::outs());

// struct MyStruct {}

```

### Syntax Rewriters

`TODO`.

## Internals

### RawSyntax

### TokenSyntax

### Trivia

### SyntaxData

### Syntax
