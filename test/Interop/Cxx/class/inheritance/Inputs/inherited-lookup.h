#pragma once

struct Base1 {
  int method(void) const { return 1; }
};

struct IBase1 : Base1 {};

struct IIBase1 : IBase1 {};
