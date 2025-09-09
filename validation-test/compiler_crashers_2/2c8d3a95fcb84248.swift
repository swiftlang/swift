// {"kind":"typecheck","signature":"swift::recordRequiredImportAccessLevelForDecl(swift::Decl const*, swift::DeclContext const*, swift::AccessLevel, std::__1::function<void (swift::AttributedImport<swift::ImportedModule>)>)"}
// RUN: %empty-directory(%t)
// RUN: not --crash %target-swift-frontend -typecheck -sdk %t %s
// REQUIRES: OS=macosx
import Distributed
distributed actor a {
}
