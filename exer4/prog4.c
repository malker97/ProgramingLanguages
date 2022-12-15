#include <stdio.h>
bool foo(bool b){
    return b;
}
int main() {
    int a = 0;
    if (foo(++a || ++a)) {
        printf("a=%d", a);
    }
    return 0;
}