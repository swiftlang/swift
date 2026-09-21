# This script produces a list of all diagnostics that are defined but not used
# in sources.

import re
from pathlib import Path

DIAG_DEF_PATTERN = re.compile(r'(?:ERROR|WARNING|NOTE|REMARK)\(([a-zA-Z0-9_]+),')
DIAG_REF_PATTERN = re.compile(r'diag::\s*([a-zA-Z0-9_]+)')
IDENT_PATTERN = re.compile(r'[a-zA-Z0-9_]+')

def diagnostics() -> set[str]:
    """Collect all diagnostic identifiers from .def files."""
    return {
        match.group(1)
        for path in Path("include/swift/AST").glob("Diagnostics*.def")
        for match in DIAG_DEF_PATTERN.finditer(path.read_text(errors="replace"))
    }

def cxx_uses() -> set[str]:
    """Collect all diag:: references from C++ source files."""
    sources = (
                  path
                  for directory in ("lib", "include", "tools")
                  for extension in ("*.cpp", "*.h")
                  for path in Path(directory).rglob(extension)
              )
    text = " ".join(source.read_text(errors="replace") for source in sources)
    return {match.group(1) for match in DIAG_REF_PATTERN.finditer(text)}

def swift_uses(candidates: set[str]) -> set[str]:
    """Return the subset of candidates that appear in Swift sources."""
    use: set[str] = set()
    for source in Path("SwiftCompilerSources").rglob("*.swift"):
        text = source.read_text(errors="replace")
        use |= candidates & {match.group() for match in IDENT_PATTERN.finditer(text)}
    return use

def main() -> None:
    unused = diagnostics() - cxx_uses()
    unused -= swift_uses(unused)
    for name in sorted(unused):
        print(name)

if __name__ == "__main__":
    main()
