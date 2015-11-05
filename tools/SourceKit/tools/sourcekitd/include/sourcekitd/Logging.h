#ifndef LLVM_SOURCEKITD_LOGGING_H
#define LLVM_SOURCEKITD_LOGGING_H

#include "llvm/Support/raw_ostream.h"

namespace sourcekitd {

void enableLogging(llvm::StringRef LoggerName);

#define DEF_COLOR(NAME, COLOR)\
static const llvm::raw_ostream::Colors NAME##Color = llvm::raw_ostream::COLOR;
DEF_COLOR(DictKey, YELLOW)
DEF_COLOR(UID, CYAN)
#undef DEF_COLOR

class OSColor {
  llvm::raw_ostream &OS;
  bool HasColors;
public:
  OSColor(llvm::raw_ostream &OS, llvm::raw_ostream::Colors Color) : OS(OS) {
    HasColors = OS.has_colors();
    if (HasColors)
      OS.changeColor(Color);
  }
  ~OSColor() {
    if (HasColors)
      OS.resetColor();
  }

  OSColor &operator<<(char C) { OS << C; return *this; }
  OSColor &operator<<(llvm::StringRef Str) { OS << Str; return *this; }
};

} // namespace sourcekitd

#endif
