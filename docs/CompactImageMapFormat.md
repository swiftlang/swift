Compact ImageMap Format
=======================

A process' address space contains (among other things) the set of
dynamically loaded images that have been mapped into that address
space.  When generating crash logs or symbolicating backtraces, we
need to be able to capture and potentially store the list of images
that has been loaded, as well as some of the attributes of those
images, including each image's

- Path
- Build ID (aka UUID)
- Base address
- End-of-text address

Compact ImageMap Format (CIF) is a binary format for holding this
information.

### General Format

Compact ImageMap Format data is byte aligned and starts with an
information byte:

~~~
   7   6   5   4   3   2   1   0
 ┌───────────────────────┬───────┐
 │ version               │ size  │
 └───────────────────────┴───────┘
~~~

The `version` field identifies the version of CIF that is in use; this
document describes version `0`. The `size` field is encoded as
follows:

| `size` | Machine word size |
| :----: | :---------------- |
|   00   | 16-bit            |
|   01   | 32-bit            |
|   10   | 64-bit            |
|   11   | Reserved          |

This is followed immediately by a field containing the name of the platform
that generated this image map.  This field consists of a single byte length
followed by a UTF-8 string of that length.

After that is a field encoding the number of images in the image map;
this field is encoded as a sequence of bytes, each holding seven bits
of data, with the top bit clear for the final byte.  The most
significant byte is the first.  e.g.

| `count` | Encoding    |
| ------: | :---------- |
|       0 | 00          |
|       1 | 01          |
|     127 | 7f          |
|     128 | 81 00       |
|     129 | 81 01       |
|     700 | 85 3c       |
|    1234 | 89 52       |
|   16384 | 81 80 00    |
|   65535 | 83 ff 7f    |
| 2097152 | 81 80 80 00 |

This in turn is followed by the list of images, stored in order of
increasing base address.  For each image, we start with a header byte:

~~~
   7   6   5   4   3   2   1   0
 ┌───┬───┬───────────┬───────────┐
 │ r │ 0 │ acount    │ ecount    │
 └───┴───┴───────────┴───────────┘
~~~

If `r` is set, then the base address is understood to be relative to
the previously computed base address.

This byte is followed by `acount + 1` bytes of base address, then
`ecount + 1` bytes of offset to the end of text.

Following this is an encoded count of bytes in the build ID,
encoded using the 7-bit scheme we used to encode the image count, and
then after that come the build ID bytes themselves.

Finally, we encode the path string using the scheme below.

### String Encoding

Image paths contain a good deal of redundancy; paths are therefore
encoded using a prefix compression scheme.  The basic idea here is
that while generating or reading the data, we maintain a mapping from
small integers to path prefix segments.

The mapping is initialised with the following fixed list that never
need to be stored in CIF data:

| code | Path prefix                         |
| :--: | :---------------------------------- |
|   0  | `/lib`                              |
|   1  | `/usr/lib`                          |
|   2  | `/usr/local/lib`                    |
|   3  | `/opt/lib`                          |
|   4  | `/System/Library/Frameworks`        |
|   5  | `/System/Library/PrivateFrameworks` |
|   6  | `/System/iOSSupport`                |
|   7  | `/Library/Frameworks`               |
|   8  | `/System/Applications`              |
|   9  | `/Applications`                     |
|  10  | `C:\Windows\System32`               |
|  11  | `C:\Program Files`                  |

Codes below 32 are reserved for future expansion of the fixed list.

Strings are encoded as a sequence of bytes, as follows:

|  `opcode`  | Mnemonic  | Meaning                                   |
| :--------: | :-------- | :---------------------------------------- |
| `00000000` | `end`     | Marks the end of the string               |
| `00xxxxxx` | `str`     | Raw string data                           |
| `01xxxxxx` | `framewk` | Names a framework                         |
| `1exxxxxx` | `expand`  | Identifies a prefix in the table          |

#### `end`

##### Encoding

~~~
   7   6   5   4   3   2   1   0
 ┌───────────────────────────────┐
 │ 0   0   0   0   0   0   0   0 │  end
 └───────────────────────────────┘
~~~

#### Meaning

Marks the end of the string

#### `str`

##### Encoding

~~~
   7   6   5   4   3   2   1   0
 ┌───────┬───────────────────────┐
 │ 0   0 │ count                 │  str
 └───────┴───────────────────────┘
~~~

##### Meaning

The next `count` bytes are included in the string verbatim.
Additionally, all path prefixes of this string data will be added to
the current prefix table.  For instance, if the string data is
`/swift/linux/x86_64/libfoo.so`, then the prefix `/swift` will be
assigned the next available code, `/swift/linux` the code after that,
and `/swift/linux/x86_64` the code following that one.

#### `framewk`

##### Encoding

~~~
   7   6   5   4   3   2   1   0
 ┌───────┬───────────────────────┐
 │ 0   1 │ count                 │  framewk
 └───────┴───────────────────────┘
~~~

##### Meaning

The next byte is a version character (normally `A`, but some
frameworks use higher characters), after which there are `count + 1`
bytes of name.

This is expanded using the pattern
`/<name>.framework/Versions/<version>/<name>`.  This also marks the
end of the string.

#### `expand`

##### Encoding

~~~
   7   6   5   4   3   2   1   0
 ┌───┬───┬───────────────────────┐
 │ 1 │ e │ code                  │  expand
 └───┴───┴───────────────────────┘
~~~

##### Meaning

If `e` is `0`, `code` is the index into the prefix table for the
prefix that should be appended to the string at this point.

If `e` is `1`, this opcode is followed by `code + 1` bytes that give
a value `v` such that `v + 64` is the index into the prefix table for
the prefix that should be appended to the string at this point.

#### Example

Let's say we wish to encode the following strings:

    /System/Library/Frameworks/AppKit.framework/Versions/C/AppKit
    /System/Library/Frameworks/Photos.framework/Versions/A/Photos
    /usr/lib/libobjc.A.dylib
    /usr/lib/libz.1.dylib
    /usr/lib/swift/libswiftCore.dylib
    /usr/lib/libSystem.B.dylib
    /usr/lib/libc++.1.dylib

We would encode

    <84> <45> CAppKit <00>

We then follow with

    <84> <45> APhotos <00>

Next we have

    <81> <10> /libobjc.A.dylib <00>
    <81> <0d> /libz.1.dylib <00>
    <81> <19> /swift/libswiftCore.dylib <00>

assigning code 32 to `/swift`, then

    <81> <12> /libSystem.B.dylib <00>
    <81> <0f> /libc++.1.dylib <00>

In total the original data would have taken up 256 bytes.  Instead, we
have used 122 bytes, a saving of over 50%.
