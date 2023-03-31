@_documentation(visibility: public)
enum E
{
}

// The @_documentation attribute caused sourcekit-lsp to crash
// cf. https://github.com/apple/swift/issues/64309

// RUN: %sourcekitd-test -req=cursor -pos=2:6 %s -- %s
