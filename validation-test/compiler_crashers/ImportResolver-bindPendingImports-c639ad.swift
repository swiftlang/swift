// {"extraArgs":["-enable-experimental-feature","BuiltinModule"],"kind":"emit-ir","signature":"(anonymous namespace)::ImportResolver::bindPendingImports()","signatureAssert":"Assertion failed: (!M->getFiles().empty() && isa<ClangModuleUnit>(M->getFiles().front())), function getTopLevelModule","signatureNext":"performImportResolution"}
// RUN: not --crash %target-swift-frontend -emit-ir -enable-experimental-feature BuiltinModule %s
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_BuiltinModule
import Builtin.a
