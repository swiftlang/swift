#pragma once

struct HasEnums {
  enum class Scoped {
    S1, S2, S3
  };

  enum Unscoped {
    U1, U2, U3
  };

  enum /* anonymous */ {
    A1, A2, A3
  };
};
