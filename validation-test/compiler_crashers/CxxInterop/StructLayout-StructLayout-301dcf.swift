// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"3ef78a11","signature":"swift::irgen::StructLayout::StructLayout(swift::irgen::IRGenModule&, std::__1::optional<swift::CanType>, swift::irgen::LayoutKind, swift::irgen::LayoutStrategy, llvm::ArrayRef<swift::irgen::TypeInfo const*>, llvm::StructType*)","signatureNext":"TypeConverter::convertStructType"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
struct a {
  var b: Cell<a>
}
