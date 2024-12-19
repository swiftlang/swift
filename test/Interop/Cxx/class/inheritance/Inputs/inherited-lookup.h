#pragma once

struct Base1 {
  int methodBase(void) const { return 1; }
};

struct IBase1 : Base1 {
  int methodIBase(void) const { return 11; }
};

struct IIBase1 : IBase1 {
  int methodIIBase(void) const { return 111; }
};
