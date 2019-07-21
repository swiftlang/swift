// Even though these are marked "closed", Swift shouldn't trust it.

enum Alpha {
  AlphaA __attribute__((swift_name("a"))),
  AlphaB __attribute__((swift_name("b"))),
  AlphaC __attribute__((swift_name("c"))),
  AlphaD __attribute__((swift_name("d"))),
  AlphaE __attribute__((swift_name("e")))
} __attribute__((enum_extensibility(closed)));

enum Coin {
  CoinHeads,
  CoinTails
} __attribute__((enum_extensibility(closed)));
