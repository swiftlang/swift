import Most

struct Post {}
struct Boast {}                   // Deprecated in Most

@available(*, deprecated, message: "got Host version")
struct Roast {}                   // Different definition in Toast
