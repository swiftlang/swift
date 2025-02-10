Compact Backtrace Format
========================

We would like to be able to efficiently store and access backtraces,
but we also wish to minimise the memory used to store them.  Since
backtraces typically contain a good deal of redundancy, it should be
possible to compress the data.

Compact Backtrace Format (CBF) is a binary format for holding a
backtrace; this specification addresses only the storage of the actual
stack backtrace, and it does not consider storage of ancillary data
(register contents, image lists and so on).  Those will be dealt with
separately elsewhere.

## General Format

Compact Backtrace Format data is byte aligned and starts with an
information byte:

~~~
   7   6   5   4   3   2   1   0
 ┌───────────────────────┬───────┐
 │ version               │ size  │
 └───────────────────────┴───────┘
~~~

The `version` field identifies the version of CBF that is in use; this
document describes version `0`. The `size` field is encqoded as
follows:

| `size` | Machine word size |
| :----: | :---------------- |
|   00   | 16-bit            |
|   01   | 32-bit            |
|   10   | 64-bit            |
|   11   | Reserved          |

This is followed by a series of instructions that tell the reader how
to decode subsequent data.

The first instruction that computes an address _must_ specify an
absolute address (the `a` bit must be set).

## Instructions

The following instructions are currently defined

|  `opcode`  | Mnemonic | Meaning                                   |
| :--------: | :------- | :---------------------------------------- |
| `00000000` | `end`    | Marks the end of the backtrace            |
| `00000001` | `trunc`  | As above, but the backtrace was truncated |
| `0000xxxx` | reserved | Reserved for future expansion             |
| `0001axxx` | `pc`     | A program counter value follows           |
| `0010axxx` | `ra`     | A return address value follows            |
| `0011axxx` | `async`  | An async resume point follows             |
| `01xxxxxx` | `omit`   | Indicates frames have been omitted        |
| `1000xxxx` | `rep`    | Repeat the previous frame                 |
| `1xxxxxxx` | reserved | Reserved for future expansion             |

If the bit labelled `a` is set, it means that the address computation
is absolute rather than being relative to the previously computed
address.

### `end`/`trunc`

#### Encoding

~~~
   7   6   5   4   3   2   1   0
 ┌───────────────────────────┬───┐
 │ 0   0   0   0   0   0   0 │ t │  end (or trunc if t is 1)
 └───────────────────────────┴───┘
~~~

#### Meaning

Marks the end of the backtrace data.  If `t` is set, it indicates that
the backtrace was truncated at this point (for instance because we hit
a frame limit while capturing).

It is not strictly necessary to use the `end` instruction if the
CBF data is of a known length.

### `pc`, `ra`, `async`

#### Encoding

~~~
   7   6   5   4   3   2   1   0
 ┌────────────────┬───┬──────────┐
 │ 0   0   0   1  │ a │ count    │ pc
 └────────────────┴───┴──────────┘
 ┌────────────────┬───┬──────────┐
 │ 0   0   1   0  │ a │ count    │ ra
 └────────────────┴───┴──────────┘
 ┌────────────────┬───┬──────────┐
 │ 0   0   1   1  │ a │ count    │ async
 └────────────────┴───┴──────────┘
~~~

#### Meaning

Each of these instructions represents a frame on the stack.  For `pc`
frames, the computed address is an actual program counter (aka
instruction pointer) value.  `ra` instructions instead represent a
_return address_, the difference being that the program has not yet
executed that instruction.  `async` instructions point at the entry
point of an async resume function, and are used when walking stacks on
systems that support `async`/`await` primitives that are implemented
by function splitting (typically an `async` instruction will point at
the start of a function containing the code immediately following an
`await`).

The next `count + 1` bytes following the instruction are an address
value.  If `a` is set, the computed address is equal to the address
value.  If `a` is not set, the computed address is equal to the
preceding computed address *plus* the address value.

Address values are sign-extended to the machine word width before
processing.  Thus a single address byte with value `0xff` on a 32-bit
backtrace represents the address value `0xffffffff`.

### `omit`

#### Encoding

~~~
   7   6   5   4   3   2   1   0
 ┌───────┬───┬───────────────────┐
 │ 0   1 │ x │ count             │ omit
 └───────┴───┴───────────────────┘
~~~

#### Meaning

Indicates that a number of frames were skipped when capturing the
backtrace.  This is used to allow a backtrace to include both the top
and bottom of the stack, without carrying every intervening frame, and
is useful to prevent the data from exploding where recursion has taken
place.

If `x` is `1`, the instruction is followed by `count + 1` bytes (up to the
machine word length) that are zero-extended to machine word length and
that represent a count of the number of frames that were omitted.

If `x` is `0`, `count + 1` is the number of frames that were omitted.

### `rep`

#### Encoding

~~~
   7   6   5   4   3   2   1   0
 ┌────────────────┬───┬──────────┐
 │ 1   0   0   0  │ x │ count    │ repeat
 └────────────────┴───┴──────────┘
~~~

#### Meaning

Repeat the previous frame.

If `x` is `1`, the instruction is followed by `count + 1` bytes that are zero
extended to machine word length and that represent a count of the number of
times to repeat the preceding frame.

If `x` is `0`, the previous frame should be repeated `count + 1` times.
