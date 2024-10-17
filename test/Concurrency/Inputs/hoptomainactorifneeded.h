
#define SWIFT_MAIN_ACTOR __attribute__((swift_attr("@MainActor")))

void useClosure(void (^block)() SWIFT_MAIN_ACTOR);
