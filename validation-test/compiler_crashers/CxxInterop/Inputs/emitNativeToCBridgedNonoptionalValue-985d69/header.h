struct S {};
typedef void (*Callback)(const S &);
void takesCallback(Callback cb);
