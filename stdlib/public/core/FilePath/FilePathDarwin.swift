/*
 This source file is part of the SE-0529 reference implementation

 Copyright (c) 2020 - 2026 Apple Inc. and the Swift System project authors
 Licensed under Apache License v2.0 with Runtime Library Exception

 See https://swift.org/LICENSE.txt for license information
*/

// MARK: - Darwin anchor parsing

// Darwin extends the basic Unix root `/` with:
// - Resolve flags: /.nofollow/, /.resolve/N/
// - Volume references: /.vol/FSID/FILEID

internal struct _ParsedDarwinAnchor {
  var anchorEnd: _SystemString.Index
  var relativeBegin: _SystemString.Index
}

extension _SystemString {
  // Try to parse a Darwin-specific anchor.
  // Returns nil if this is just a plain `/` root.
  internal func _parseDarwinAnchor() -> _ParsedDarwinAnchor? {
    _internalInvariant(_isDarwin)
    guard !isEmpty else { return nil }
    guard self.first == ._slash else { return nil }

    let afterSlash = index(after: startIndex)
    guard afterSlash < endIndex else { return nil }

    // Must start with /.
    guard self[afterSlash] == ._dot else { return nil }

    // Check for /.nofollow/
    if _matchesNofollow(from: afterSlash) {
      return _parseNofollow(from: afterSlash)
    }

    // Check for /.resolve/N/
    if _matchesResolve(from: afterSlash) {
      return _parseResolve(from: afterSlash)
    }

    // Check for /.vol/FSID/FILEID
    if _matchesVol(from: afterSlash) {
      return _parseVol(from: afterSlash)
    }

    return nil
  }

  // MARK: - /.nofollow/

  private func _matchesNofollow(from dotIdx: Index) -> Bool {
    let nofollow: [FilePath.CodeUnit] = ".nofollow".unicodeScalars.map {
      FilePath.CodeUnit(_ascii: $0)
    }
    let slice = self[dotIdx...]
    return slice.starts(with: nofollow)
  }

  private func _parseNofollow(from dotIdx: Index) -> _ParsedDarwinAnchor? {
    let nofollowLen = ".nofollow".count // 9
    let nofollowEnd = index(dotIdx, offsetBy: nofollowLen)
    guard nofollowEnd < endIndex else { return nil }
    guard self[nofollowEnd] == ._slash else { return nil }

    let afterSlash = index(after: nofollowEnd)
    return _ParsedDarwinAnchor(
      anchorEnd: afterSlash,
      relativeBegin: afterSlash)
  }

  // MARK: - /.resolve/N/

  private func _matchesResolve(from dotIdx: Index) -> Bool {
    let resolve: [FilePath.CodeUnit] = ".resolve".unicodeScalars.map {
      FilePath.CodeUnit(_ascii: $0)
    }
    let slice = self[dotIdx...]
    return slice.starts(with: resolve)
  }

  private func _parseResolve(from dotIdx: Index) -> _ParsedDarwinAnchor? {
    let resolveLen = ".resolve".count // 8
    let resolveEnd = index(dotIdx, offsetBy: resolveLen)
    guard resolveEnd < endIndex else { return nil }
    guard self[resolveEnd] == ._slash else { return nil }

    // Read the flag value (everything between the two slashes)
    let flagStart = index(after: resolveEnd)
    guard flagStart < endIndex else { return nil }

    // Find the closing slash
    guard let flagEnd = self[flagStart...].firstIndex(of: ._slash) else {
      return nil
    }
    guard flagStart < flagEnd else { return nil }

    let afterSlash = index(after: flagEnd)
    return _ParsedDarwinAnchor(
      anchorEnd: afterSlash,
      relativeBegin: afterSlash)
  }

  // MARK: - /.vol/FSID/FILEID

  private func _matchesVol(from dotIdx: Index) -> Bool {
    let vol: [FilePath.CodeUnit] = ".vol".unicodeScalars.map {
      FilePath.CodeUnit(_ascii: $0)
    }
    let slice = self[dotIdx...]
    return slice.starts(with: vol)
  }

  private func _parseVol(from dotIdx: Index) -> _ParsedDarwinAnchor? {
    let volLen = ".vol".count // 4
    let volEnd = index(dotIdx, offsetBy: volLen)
    guard volEnd < endIndex else { return nil }
    guard self[volEnd] == ._slash else { return nil }

    // Read FSID
    let fsidStart = index(after: volEnd)
    guard fsidStart < endIndex else { return nil }
    guard let fsidEnd = self[fsidStart...].firstIndex(of: ._slash) else {
      return nil
    }
    guard fsidStart < fsidEnd else { return nil }

    // Read FILEID
    let fileidStart = index(after: fsidEnd)
    guard fileidStart < endIndex else { return nil }

    // FILEID goes up to next slash or end
    let fileidEnd = self[fileidStart...].firstIndex(of: ._slash) ?? endIndex
    guard fileidStart < fileidEnd else { return nil }

    // anchorEnd is at fileidEnd (the anchor is /.vol/FSID/FILEID without trailing /)
    let relBegin: _SystemString.Index
    if fileidEnd < endIndex && self[fileidEnd] == ._slash {
      relBegin = index(after: fileidEnd)
    } else {
      relBegin = fileidEnd
    }

    return _ParsedDarwinAnchor(
      anchorEnd: fileidEnd,
      relativeBegin: relBegin)
  }
}

// MARK: - Darwin anchor canonicalization

extension _SystemString {
  // /.resolve/1/ -> /.nofollow/
  // /.vol/NNNN/2/ -> /.vol/NNNN/@/
  internal mutating func _canonicalizeDarwinAnchor() {
    guard _isDarwin else { return }

    // Check for /.resolve/1/ -> /.nofollow/
    let resolveOnePrefix: [FilePath.CodeUnit] = "/.resolve/1/".unicodeScalars.map {
      FilePath.CodeUnit(_ascii: $0)
    }
    if self.starts(with: resolveOnePrefix) {
      let nofollowPrefix: [FilePath.CodeUnit] = "/.nofollow/".unicodeScalars.map {
        FilePath.CodeUnit(_ascii: $0)
      }
      let resolveEnd = self.index(startIndex, offsetBy: resolveOnePrefix.count)
      self.replaceSubrange(startIndex..<resolveEnd, with: nofollowPrefix)
      return
    }

    // Check for /.vol/NNNN/2 -> /.vol/NNNN/@
    let volPrefix: [FilePath.CodeUnit] = "/.vol/".unicodeScalars.map {
      FilePath.CodeUnit(_ascii: $0)
    }
    guard self.starts(with: volPrefix) else { return }

    let fsidStart = self.index(startIndex, offsetBy: volPrefix.count)
    guard fsidStart < endIndex else { return }
    guard let fsidEnd = self[fsidStart...].firstIndex(of: ._slash) else {
      return
    }
    guard fsidStart < fsidEnd else { return }

    let fileidStart = self.index(after: fsidEnd)
    guard fileidStart < endIndex else { return }

    // Find end of fileid
    let fileidEnd = self[fileidStart...].firstIndex(of: ._slash) ?? endIndex

    // Check if fileid is exactly "2"
    let fileidSlice = self[fileidStart..<fileidEnd]
    let two: [FilePath.CodeUnit] = [FilePath.CodeUnit(_ascii: "2")]
    if fileidSlice.elementsEqual(two) {
      let atSign: [FilePath.CodeUnit] = [._at]
      self.replaceSubrange(fileidStart..<fileidEnd, with: atSign)
    }
  }
}

// MARK: - Resource fork detection

extension _SystemString {
  // The resource fork suffix is exactly "/..namedfork/rsrc" (17 bytes)
  internal static let _resourceForkSuffix: [FilePath.CodeUnit] =
    "/..namedfork/rsrc".unicodeScalars.map { FilePath.CodeUnit(_ascii: $0) }

  internal func _hasResourceForkSuffix() -> Bool {
    guard _isDarwin else { return false }
    let suffix = _SystemString._resourceForkSuffix
    guard self.count >= suffix.count else { return false }

    let suffixStart = self.index(endIndex, offsetBy: -suffix.count)
    return self[suffixStart...].elementsEqual(suffix)
  }

  internal var _resourceForkSuffixStart: _SystemString.Index? {
    guard _hasResourceForkSuffix() else { return nil }
    let suffix = _SystemString._resourceForkSuffix
    return self.index(endIndex, offsetBy: -suffix.count)
  }
}
