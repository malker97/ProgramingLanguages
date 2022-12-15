#include <stdio.h>
void f(int i, int j) {
    printf("i=%d, j=%d\n", i, j);
}
int main() {
    int a = 0, x, y;
    f(x=++a,y=++a);
    return 0;
}