# ByteTree

The ByteTree format is a binary format to efficiently serialize and deserialize trees. It was designed to serialize the syntax tree in `libSyntax` but the framework allows serialisation of arbitrary trees. It currently offers a serialiser written in C++ and a deserialiser written in Swift.

## Overview
The ByteTree format consists of two different constructs: *objects* and *scalars*. A scalar is a raw sequence of binary data. Scalars are untyped and the meaning of their binary data needs to be inferred by the client based on their position in the tree. An object consists of multiple *fields*, indexed by their position within the object, which again can be either objects or scalars. 

## Serialization of scalars

A scalar is encoded as its size followed by the data. Size is a `uint_32` that represents the size of the data in bytes in little endian order.

For example, the string "Hello World" would be encoded as `(uint32_t)11` `"Hello World"`, or in hex `0B 00 00 00   48 65 6C 6C 6F 20 57 6F 72 6C 64`.

## Serialization of objects

An object consists of its size, measured in the number of fields and represented as a `uint_32t` in little endian order, followed by the direct concatenation of its fields. Because each field is again prefixed with its size, no delimites are necessary in between the fields.

Arrays are modelled as objects whose fields are all of the same type and whose length is variadic (and is indicated by the object's size).

## Versioning

The ByteTree format is prepended by a 4-byte protocol version number that describes the version of the object tree that was serialized. Its exact semantics are up to each specific application, but it is encouraged to interpret it as a two-comentent number where the first component, consisting of the first three bytes, is incremented for breaking changes and the last byte is incremented for backwards-compatible changes.

## Serialization safety

Since all fields in objects are accessed by their index, issues quickly arise if a new field is accidentally added at the beginning of an object. To prevent issues like this, the ByteTree serialiser and deserialiser requires the explicit specification of each field's index within the object. These indicies are never serialised. Their sole purpose is to check that all fields are read in the correct order in assertion builds.
