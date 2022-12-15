#include <stdio.h>
void f(int i, int j) {
    printf("i=%d, j=%d\n", i, j);
}
int main() {
    int a = 0;
    f(++a,++a);
    return 0;
}