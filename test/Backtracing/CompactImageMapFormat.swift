// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -Xfrontend -disable-availability-checking -Onone -o %t/CompactImageMapFormat.exe
// RUN: %target-codesign %t/CompactImageMapFormat.exe
// RUN: %target-run %t/CompactImageMapFormat.exe | %FileCheck %s

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=macosx || OS=linux-gnu || OS=windows-msvc

// Comprehensive, byte-level tests for the Compact Image Map Format decoder
// and encoder (see docs/CompactImageMapFormat.md).  Rather than relying on
// a live-captured ImageMap (see CompactImageMap.swift for that), these
// tests hand-construct CIMF byte streams so that specific valid and
// invalid encodings can be exercised directly.

@_spi(Internal) import Runtime

var failures = 0

func report(_ name: String, _ ok: Bool, _ detail: String = "") {
  if ok {
    print("PASS: \(name)")
  } else {
    failures += 1
    print("FAIL: \(name) \(detail)")
  }
}

// MARK: - Byte stream builders

func encode7BitCount(_ value: Int) -> [UInt8] {
  var chunks: [UInt8] = [UInt8(value & 0x7f)]
  var v = value >> 7
  while v > 0 {
    chunks.append(UInt8(v & 0x7f))
    v >>= 7
  }
  var bytes: [UInt8] = []
  for (i, c) in chunks.reversed().enumerated() {
    if i != chunks.count - 1 {
      bytes.append(c | 0x80)
    } else {
      bytes.append(c)
    }
  }
  return bytes
}

func infoByte(version: UInt8 = 0, size: UInt8) -> UInt8 {
  return (version << 2) | size
}

func platformField(_ s: String) -> [UInt8] {
  let utf8 = Array(s.utf8)
  return [UInt8(utf8.count)] + utf8
}

func bigEndianBytes(_ value: UInt64, count: Int) -> [UInt8] {
  var out: [UInt8] = []
  for i in stride(from: count - 1, through: 0, by: -1) {
    out.append(UInt8((value >> (8 * i)) & 0xff))
  }
  return out
}

func pathStr(_ s: String) -> [UInt8] {
  var out: [UInt8] = []
  let bytes = Array(s.utf8)
  var idx = 0
  while idx < bytes.count {
    let chunk = min(bytes.count - idx, 0x3f)
    out.append(UInt8(chunk))
    out.append(contentsOf: bytes[idx..<idx+chunk])
    idx += chunk
  }
  return out
}

func pathEnd() -> [UInt8] { [0x00] }
func pathNone() -> [UInt8] { [0x00] }

func pathFramework(name: String, version: UInt8) -> [UInt8] {
  let nameBytes = Array(name.utf8)
  precondition(nameBytes.count >= 1 && nameBytes.count <= 64)
  var out: [UInt8] = [0x40 | UInt8(nameBytes.count - 1), version]
  out.append(contentsOf: nameBytes)
  return out
}

func pathExpandDirect(_ code: Int) -> [UInt8] {
  precondition(code >= 0 && code <= 0x3f)
  return [0x80 | UInt8(code)]
}

func pathExpandExtended(_ code: Int) -> [UInt8] {
  precondition(code >= 64)
  var v = UInt64(code - 64)
  var bytes: [UInt8] = []
  repeat {
    bytes.append(UInt8(v & 0xff))
    v >>= 8
  } while v > 0
  bytes.reverse()
  precondition(bytes.count <= 64)
  var out: [UInt8] = [0xc0 | UInt8(bytes.count - 1)]
  out.append(contentsOf: bytes)
  return out
}

struct ImageDesc {
  var relative: Bool = false
  var addressBytes: [UInt8]
  var eotBytes: [UInt8]
  var buildId: [UInt8]? = nil
  var pathBytes: [UInt8]

  func encode() -> [UInt8] {
    let acount = addressBytes.count
    let ecount = eotBytes.count
    precondition(acount >= 1 && acount <= 8)
    precondition(ecount >= 1 && ecount <= 8)
    var out: [UInt8] = []
    var header: UInt8 = relative ? 0x80 : 0x00
    header |= UInt8((acount - 1) << 3)
    header |= UInt8(ecount - 1)
    out.append(header)
    out.append(contentsOf: addressBytes)
    out.append(contentsOf: eotBytes)
    let idBytes = buildId ?? []
    out.append(contentsOf: encode7BitCount(idBytes.count))
    out.append(contentsOf: idBytes)
    out.append(contentsOf: pathBytes)
    return out
  }
}

func cifData(size: UInt8 = 2, platform: String = "test",
             imageCount: Int? = nil, images: [ImageDesc] = [],
             extra: [UInt8] = []) -> [UInt8] {
  var out: [UInt8] = [infoByte(size: size)]
  out.append(contentsOf: platformField(platform))
  out.append(contentsOf: encode7BitCount(imageCount ?? images.count))
  for image in images {
    out.append(contentsOf: image.encode())
  }
  out.append(contentsOf: extra)
  return out
}

func infoByteAndPlatform(size: UInt8 = 2, platform: String = "test") -> [UInt8] {
  return [infoByte(size: size)] + platformField(platform)
}

// MARK: - Expectation helpers

func expectDecodeFails(_ name: String, _ bytes: [UInt8]) {
  let map = ImageMap(compactImageMapData: bytes)
  report(name, map == nil, "expected nil, got \(String(describing: map))")
}

@discardableResult
func expectDecodeSucceeds(_ name: String, _ bytes: [UInt8]) -> ImageMap? {
  let map = ImageMap(compactImageMapData: bytes)
  report(name, map != nil, "expected non-nil decode")
  return map
}

@main
struct CompactImageMapFormatTest {
  static func main() {

// MARK: - Valid inputs

// 1. Basic valid round trip, single image, absolute address, build ID, str path
do {
  let img = ImageDesc(
    addressBytes: bigEndianBytes(0x0000_5555_0000_1000, count: 8),
    eotBytes: bigEndianBytes(0x2000, count: 2),
    buildId: [0xde, 0xad, 0xbe, 0xef],
    pathBytes: pathStr("/usr/lib/libfoo.dylib") + pathEnd()
  )
  let bytes = cifData(size: 2, platform: "macosx", images: [img])
  if let map = expectDecodeSucceeds("basic valid decode", bytes) {
    let encoded = Array(CompactImageMapFormat.Encoder(map))
    if let map2 = expectDecodeSucceeds("basic round-trip re-decode", encoded) {
      report("basic round-trip matches", map == map2, "\(map) != \(map2)")
    }
  }
}

// 2. Path with "no path at all"
do {
  let img = ImageDesc(
    addressBytes: bigEndianBytes(0x1000, count: 2),
    eotBytes: bigEndianBytes(0x100, count: 2),
    pathBytes: pathNone()
  )
  let bytes = cifData(images: [img])
  if let map = expectDecodeSucceeds("no-path image decodes", bytes) {
    report("no-path image has nil path", map[0].path == nil)
  }
}

// 3. Fixed prefix expand (code 0 = /lib)
do {
  let img = ImageDesc(
    addressBytes: bigEndianBytes(0x1000, count: 2),
    eotBytes: bigEndianBytes(0x100, count: 2),
    pathBytes: pathExpandDirect(0) + pathStr("/libbar.so") + pathEnd()
  )
  let bytes = cifData(images: [img])
  if let map = expectDecodeSucceeds("fixed-prefix expand decodes", bytes) {
    report("fixed-prefix expand value", map[0].path == "/lib/libbar.so",
           "got \(String(describing: map[0].path))")
  }
}

// 4. Dynamic prefix definition + expand
do {
  let img1 = ImageDesc(
    addressBytes: bigEndianBytes(0x1000, count: 2),
    eotBytes: bigEndianBytes(0x100, count: 2),
    pathBytes: pathStr("/swift/linux/x86_64/libfoo.so") + pathEnd()
  )
  let img2 = ImageDesc(
    relative: true,
    addressBytes: bigEndianBytes(0x100, count: 2),
    eotBytes: bigEndianBytes(0x100, count: 2),
    pathBytes: pathExpandDirect(33) + pathStr("/libbar.so") + pathEnd()
  )
  let bytes = cifData(images: [img1, img2])
  if let map = expectDecodeSucceeds("dynamic-prefix decodes", bytes) {
    report("dynamic prefix img1 path", map[0].path == "/swift/linux/x86_64/libfoo.so",
           "got \(String(describing: map[0].path))")
    report("dynamic prefix img2 path", map[1].path == "/swift/linux/libbar.so",
           "got \(String(describing: map[1].path))")
    report("relative address applied", map[1].baseAddress == 0x1100,
           "got \(map[1].baseAddress)")
  }
}

// 5. Framework path expansion
do {
  let img = ImageDesc(
    addressBytes: bigEndianBytes(0x1000, count: 2),
    eotBytes: bigEndianBytes(0x100, count: 2),
    pathBytes: pathExpandDirect(4) + pathFramework(name: "AppKit", version: UInt8(ascii: "C"))
  )
  let bytes = cifData(images: [img])
  if let map = expectDecodeSucceeds("framework path decodes", bytes) {
    let expected = "/System/Library/Frameworks/AppKit.framework/Versions/C/AppKit"
    report("framework path value", map[0].path == expected,
           "got \(String(describing: map[0].path))")
    report("framework name extraction", map[0].name == "AppKit",
           "got \(String(describing: map[0].name))")
  }
}

// 6. Extended (e=1) expand referencing a dynamically defined prefix >= 64
do {
  let letters = (0..<26).map { Character(UnicodeScalar(UInt8(ascii: "A") + UInt8($0))) }
             + (0..<14).map { Character(UnicodeScalar(UInt8(ascii: "a") + UInt8($0))) }
  let longPath = "/" + letters.map(String.init).joined(separator: "/")
  let sBytes = Array(longPath.utf8)
  precondition(sBytes.count == 80)
  // Split into two `str` chunks (max 63 bytes each) so the prefixes still
  // accumulate against a single stringBase.
  func rawStr(_ bytes: ArraySlice<UInt8>) -> [UInt8] {
    return [UInt8(bytes.count)] + Array(bytes)
  }
  let img1 = ImageDesc(
    addressBytes: bigEndianBytes(0x1000, count: 2),
    eotBytes: bigEndianBytes(0x100, count: 2),
    pathBytes: rawStr(sBytes[0..<63]) + rawStr(sBytes[63...]) + pathEnd()
  )
  let img2 = ImageDesc(
    relative: true,
    addressBytes: bigEndianBytes(0x100, count: 2),
    eotBytes: bigEndianBytes(0x100, count: 2),
    pathBytes: pathExpandExtended(64) + pathStr("/extra.so") + pathEnd()
  )
  let bytes = cifData(images: [img1, img2])
  if let map = expectDecodeSucceeds("extended expand decodes", bytes) {
    report("extended expand long path", map[0].path == longPath,
           "got \(String(describing: map[0].path))")
    let expectedPrefix = "/A/B/C/D/E/F/G/H/I/J/K/L/M/N/O/P/Q/R/S/T/U/V/W/X/Y/Z/a/b/c/d/e/f/g"
    report("extended expand code64 value", map[1].path == expectedPrefix + "/extra.so",
           "got \(String(describing: map[1].path))")
  }
}

// MARK: - Invalid input: empty / truncated header

expectDecodeFails("empty data", [])
expectDecodeFails("nonzero version rejected", [infoByte(version: 1, size: 2)])
expectDecodeFails("reserved size rejected", [infoByte(version: 0, size: 3)])
expectDecodeFails("truncated after info byte", [infoByte(size: 2)])

// MARK: - Invalid input: platform string

expectDecodeFails("platform string truncated",
  [infoByte(size: 2), 0x05, 0x41, 0x42]) // claims 5 bytes, only 2 given

expectDecodeFails("platform string invalid utf8",
  [infoByte(size: 2), 0x01, 0xff] + encode7BitCount(0))

// MARK: - Invalid input: image count

expectDecodeFails("image count exceeds max",
  infoByteAndPlatform() + encode7BitCount(1_048_577))

expectDecodeFails("image count negative via overflow",
  infoByteAndPlatform() + [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f])

expectDecodeSucceeds("image count at max boundary with zero images",
  infoByteAndPlatform() + encode7BitCount(0))

// MARK: - Invalid input: build ID length

do {
  var bytes = infoByteAndPlatform() + encode7BitCount(1)
  bytes += [0x00] // header: acount=1, ecount=1, absolute
  bytes += bigEndianBytes(0x1000, count: 1)
  bytes += bigEndianBytes(0x100, count: 1)
  bytes += encode7BitCount(1025) // exceeds maxBuildIdLength
  expectDecodeFails("build ID length exceeds max", bytes)
}

do {
  var bytes = infoByteAndPlatform() + encode7BitCount(1)
  bytes += [0x00]
  bytes += bigEndianBytes(0x1000, count: 1)
  bytes += bigEndianBytes(0x100, count: 1)
  bytes += [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f] // overflow -> negative
  expectDecodeFails("build ID length negative via overflow", bytes)
}

// MARK: - Invalid input: end-of-text offset

do {
  // ecount = 1, offset byte = 0x00 -> endOfText == baseAddress -> rejected
  let img = ImageDesc(addressBytes: bigEndianBytes(0x1000, count: 2),
                       eotBytes: [0x00],
                       pathBytes: pathNone())
  expectDecodeFails("zero end-of-text offset rejected", cifData(images: [img]))
}

do {
  // ecount = 1, offset byte = 0xff, size = 64-bit -> sign-extends to -1.
  // baseAddress (0x1000) &+ (-1 as UInt64) wraps to 0xfff, which is below
  // baseAddress -> rejected. (Using a 1-byte *address* here would instead
  // truncate 0x1000 down to 0x00, hiding the wraparound -- the address must
  // be wide enough to survive intact for this test to be meaningful.)
  let img = ImageDesc(addressBytes: bigEndianBytes(0x1000, count: 2),
                       eotBytes: [0xff],
                       pathBytes: pathNone())
  expectDecodeFails("negative end-of-text offset rejected",
                     cifData(size: 2, images: [img]))
}

// MARK: - Invalid input: excessive number of images (declared vs actual)

expectDecodeFails("declared image count larger than actual data",
  infoByteAndPlatform() + encode7BitCount(5)) // no image data follows

// MARK: - Invalid input: over-long / truncated strings in paths
//
// decodePath() returns nil on *any* internal failure (truncation, bad
// opcode, bad UTF-8, ...), and the caller treats that the same as "no path
// was present at all" -- it does not fail the overall decode. So a
// truncated path (when it's also the last thing in the stream) yields a
// successful decode with `.path == nil` / `.name == nil`, not an overall
// decode failure. This is intentional-looking but easy to miss, so we
// pin it down explicitly here.

do {
  // `str` opcode claims 10 bytes but stream ends early
  let img = ImageDesc(addressBytes: bigEndianBytes(0x1000, count: 2),
                       eotBytes: bigEndianBytes(0x100, count: 2),
                       pathBytes: [0x0a, 0x41, 0x42]) // count=10, only 2 bytes given
  if let map = expectDecodeSucceeds("path str truncated still decodes overall", cifData(images: [img])) {
    report("path str truncated yields nil path", map[0].path == nil,
           "got \(String(describing: map[0].path))")
  }
}

do {
  // `framewk` opcode claims 20 bytes of name but stream ends early
  let img = ImageDesc(addressBytes: bigEndianBytes(0x1000, count: 2),
                       eotBytes: bigEndianBytes(0x100, count: 2),
                       pathBytes: [0x40 | 19, UInt8(ascii: "A"), UInt8(ascii: "x")])
  if let map = expectDecodeSucceeds("path framework name truncated still decodes overall", cifData(images: [img])) {
    report("path framework truncated yields nil path", map[0].path == nil,
           "got \(String(describing: map[0].path))")
  }
}

do {
  // `str` opcode claims *far* more bytes than are really meant for this
  // path (20), with no terminating `end` -- so the decoder keeps consuming
  // raw stream bytes (which actually belong to the next image's header,
  // address, etc.) as part of this "string". That eventually exhausts the
  // whole byte sequence partway through image 2's fields, so the *overall*
  // decode fails safely rather than silently producing a corrupted image
  // map -- there's no per-field boundary check, just eventual exhaustion.
  let img1 = ImageDesc(addressBytes: bigEndianBytes(0x1000, count: 2),
                        eotBytes: bigEndianBytes(0x100, count: 2),
                        pathBytes: [20, UInt8(ascii: "A"), UInt8(ascii: "B"), UInt8(ascii: "C")])
  let img2 = ImageDesc(addressBytes: bigEndianBytes(0x9999, count: 2),
                        eotBytes: bigEndianBytes(0x10, count: 2),
                        pathBytes: pathStr("/Z") + pathEnd())
  expectDecodeFails("over-length str opcode overruns into next image and fails safely",
                     cifData(images: [img1, img2]))
}

// MARK: - Invalid input: invalid UTF-8 in path strings
// NOTE: decodePath() conflates "invalid UTF-8" with "no path at all" because
// both return nil; document actual observed behaviour here.

do {
  let img = ImageDesc(addressBytes: bigEndianBytes(0x1000, count: 2),
                       eotBytes: bigEndianBytes(0x100, count: 2),
                       pathBytes: [0x01, 0xff] + pathEnd())
  if let map = expectDecodeSucceeds("path with invalid utf8 still decodes overall", cifData(images: [img])) {
    report("path with invalid utf8 yields nil path", map[0].path == nil,
           "got \(String(describing: map[0].path))")
  }
}

// MARK: - Invalid input: out-of-range expand codes
//
// As with the truncation cases above, these are all internal decodePath()
// failures, so they surface as `.path == nil` on an otherwise-successful
// decode, not as an overall decode failure.

do {
  // code 20 is in the reserved range (12-31), never assigned
  let img = ImageDesc(addressBytes: bigEndianBytes(0x1000, count: 2),
                       eotBytes: bigEndianBytes(0x100, count: 2),
                       pathBytes: pathExpandDirect(20) + pathEnd())
  if let map = expectDecodeSucceeds("expand of undefined reserved code still decodes overall", cifData(images: [img])) {
    report("expand of undefined reserved code yields nil path", map[0].path == nil,
           "got \(String(describing: map[0].path))")
  }
}

do {
  // code 40 (dynamic range) never defined in this stream
  let img = ImageDesc(addressBytes: bigEndianBytes(0x1000, count: 2),
                       eotBytes: bigEndianBytes(0x100, count: 2),
                       pathBytes: pathExpandDirect(40) + pathEnd())
  if let map = expectDecodeSucceeds("expand of undefined dynamic code still decodes overall", cifData(images: [img])) {
    report("expand of undefined dynamic code yields nil path", map[0].path == nil,
           "got \(String(describing: map[0].path))")
  }
}

do {
  // Extended expand (e=1) arithmetic overflow: code + 64 overflows Int64
  let img = ImageDesc(addressBytes: bigEndianBytes(0x1000, count: 2),
                       eotBytes: bigEndianBytes(0x100, count: 2),
                       pathBytes: [0xc7, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xc0])
  if let map = expectDecodeSucceeds("expand code arithmetic overflow still decodes overall", cifData(images: [img])) {
    report("expand code arithmetic overflow yields nil path", map[0].path == nil,
           "got \(String(describing: map[0].path))")
  }
}

do {
  // Extended expand truncated mid-code
  let img = ImageDesc(addressBytes: bigEndianBytes(0x1000, count: 2),
                       eotBytes: bigEndianBytes(0x100, count: 2),
                       pathBytes: [0xc3, 0x00, 0x00]) // claims 4 bytes, only 2 given
  if let map = expectDecodeSucceeds("expand extended code truncated still decodes overall", cifData(images: [img])) {
    report("expand extended code truncated yields nil path", map[0].path == nil,
           "got \(String(describing: map[0].path))")
  }
}

if failures == 0 {
  print("ALL TESTS PASSED")
} else {
  print("\(failures) TESTS FAILED")
}

  }
}

// CHECK-NOT: FAIL
// CHECK: ALL TESTS PASSED
