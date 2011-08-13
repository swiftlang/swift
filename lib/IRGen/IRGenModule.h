namespace llvm { class TargetData; }

namespace swift {
  struct IRGenModule {
  IRGenModule(ASTContext &C, irgen::Options &Opts, llvm::Module&, const llvm::TargetData&) {}
  void emitTranslationUnit(TranslationUnitDecl *TU) ;
  };

}
