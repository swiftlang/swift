
typedef struct {
	int value;
} MyTime;

extern const MyTime kMyTimeZero;

extern MyTime MyTimeAdd(MyTime lhs, MyTime rhs);
