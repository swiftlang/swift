# Random APIs

More documentation to come.

## Platform-Specific Default Random

The implementation of the default random generator varies by platform. The implementation
on each platform must be thread-safe and automatically seeded, and should be
cryptographically secure to the extent possible. Currently supported platforms have the
following implementation details:

- Apple platforms use `arc4random_buf(3)`.
- Linux, FreeBSD, and other UNIX-like platforms use `getrandom(2)` when available;
otherwise, they read from `/dev/urandom`.
- Fuchsia platforms use `getentropy(3)`.
- Windows platforms use `BCryptGenRandom`.

Embedded Swift does not link the runtime above; the embedded standard library
carries its own implementation. On Linux it calls `arc4random_buf(3)` when the C
library provides it (glibc 2.36 and later) and `getrandom(2)` otherwise. When
the Embedded Swift platform abstraction layer is in use, the platform supplies
the generator instead, through `_swift_generateRandom`.
